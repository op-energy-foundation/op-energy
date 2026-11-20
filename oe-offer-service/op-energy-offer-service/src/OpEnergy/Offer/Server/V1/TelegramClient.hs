{-- | Follows the telegram group the trade chat is held in, and keeps a copy
 - of what is said there.
 -
 - The group is read by long polling: telegram is asked to hold each request
 - open until it has something to say, and answers at once when it does, so
 - a message reaches the copy within a second of being sent without this
 - service asking again and again.
 -
 - Everything one poll returns is written in a single transaction together
 - with the offset, which covers it. A crash between the two leaves telegram
 - redelivering what it already sent rather than skipping it, and the unique
 - constraint on the message table absorbs the redelivery, so there is no
 - recovery path here to get wrong.
 -
 - 'runTelegramMirror' never returns and never throws a synchronous
 - exception: the process waits on any of its threads ending, so a mirror,
 - which gave up, would take settlement and the API down with it.
 -}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
module OpEnergy.Offer.Server.V1.TelegramClient
  ( runTelegramMirror
  ) where

import           Control.Concurrent (threadDelay)
import           Control.Monad (forever, when)
import           Data.IORef (IORef, newIORef, readIORef, writeIORef)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Logger (logInfo, logWarn)
import           Control.Monad.Trans.Reader (ask)
import qualified Control.Exception.Safe as E
import           Data.Int (Int64)
import           Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Prometheus (MonadMonitor)

import           Data.Text.Show (tshow)

import           OpEnergy.Offer.Server.V1.Class
                 ( AppT
                 , State(..)
                 , runAppT
                 , runLogging
                 , withDBTransaction
                 )
import           OpEnergy.Offer.Server.V1.Config
                 ( Config(..)
                 , TelegramBotToken(..)
                 )
import           OpEnergy.Offer.Server.V1.Telegram.Client
                 ( BotPath(..)
                 , ChatMember(..)
                 , ChatMemberRequest(..)
                 , GetUpdatesRequest(..)
                 , SendMessageRequest(..)
                 , Message(..)
                 , TelegramChat(..)
                 , TelegramUser(..)
                 , Update(..)
                 , defaultGetUpdatesRequest
                 , getUpdates
                 , getChatMember
                 , botUserIdOf
                 , MessagePresence(..)
                 , messageStillThere
                 , isGroupMessage
                 , isPrivateMessage
                 , sendMessage
                 , resultOf
                 , senderDisplayNameOf
                 , withTelegram
                 )
import           OpEnergy.Offer.Server.V1.LiveEvent (LiveEvent(..))
import           OpEnergy.Offer.Server.V1.Time (getCurrentTimeDB)
import           OpEnergy.Offer.Server.V1.WebSocketService (publishLiveEvent)
import           Data.OpEnergy.Offer.API.V1.LiveMessage (LiveMessage(..))
import           Data.OpEnergy.Offer.API.V1.TelegramMessageID
                 ( TelegramMessageID(..)
                 )
import           OpEnergy.Offer.Server.V1.TelegramLink (redeemLinkCodeTx)
import           OpEnergy.Offer.Server.V1.TelegramMessage
                 ( TelegramGroupMessage(..)
                 , advancePollStateTx
                 , touchPollStateTx
                 , getPollOffsetTx
                 , insertTelegramMessageTx
                 , markTelegramMessageGoneTx
                 , messagesToReconcileTx
                 , telegramMessageInfoOf
                 , updateTelegramMessageBodyTx
                 )
import           Data.OpEnergy.API.V1.Positive (fromPositive)
import           OpEnergy.Error (CallstackError, describeError, dbQueryError)

-- | how long to wait before polling again after a failure.
--
-- Long enough that an outage, or a token telegram has stopped accepting,
-- does not turn into a request every second for as long as it lasts. A
-- successful poll is followed by the next one at once, so this delay is
-- only ever paid on the way out of a failure.
retryDelayMicroseconds :: Int
retryDelayMicroseconds = 15 * 1000000

-- | how often to ask telegram whether the bot is still in the group.
--
-- This is the only way to find out. When a bot is removed from a group,
-- getUpdates does not fail: it goes on answering, successfully and for
-- ever, that there is nothing new. A mirror, which has been thrown out of
-- the room, looks exactly like a quiet room, so the question has to be
-- asked rather than waited for.
membershipCheckSeconds :: Double
membershipCheckSeconds = 300

-- | how often to ask telegram which of the recent messages are still there.
--
-- A message removed in the telegram app is removed silently -- there is no
-- deletion update of any kind -- so a mirror, which is to match the group,
-- has to ask. Until it does, the feed goes on showing something the group
-- no longer has, which is the one way this chat can mislead a reader.
--
-- Two minutes is a compromise. Sooner costs requests for a thing, which
-- happens rarely; later leaves a removed message on the site long enough to
-- be read by somebody it was removed from.
reconcileSeconds :: Double
reconcileSeconds = 120

-- | how many of the newest messages one sweep asks about.
--
-- The feed shows the recent end of the group, so the recent end is what has
-- to be right. Sweeping the whole history would grow without limit and
-- would spend every sweep on messages nobody is looking at.
reconcileBatch :: Int
reconcileBatch = 50

-- | how long to leave between the probes of one sweep.
--
-- Telegram rate limits a bot on everything it asks, and a sweep is not
-- urgent: spreading 50 probes over ten seconds keeps this well inside any
-- limit and well inside its own interval.
reconcileSpacingMicroseconds :: Int
reconcileSpacingMicroseconds = 200000

-- | how many messages in a row telegram may call gone before the sweep
-- stops believing it.
--
-- The classifier reads only telegram's two exact refusals as "gone", so
-- this should never be reached. It is here because hiding a message cannot
-- be undone and these rows are the only copy of the chat there will ever
-- be: if some answer this does not anticipate were ever read as "gone",
-- this is what keeps the mistake to a handful of messages rather than the
-- whole feed. Moderation removes a message here and there, never a dozen
-- consecutively, so a run that long is evidence about the bot rather than
-- about the group.
reconcileGoneLimit :: Int
reconcileGoneLimit = 10

-- | one message of the group, as it is about to be stored
data MirroredMessage = MirroredMessage
  { mirroredRow :: TelegramGroupMessage
  , mirroredIsEdit :: Bool
  }

-- | Follows the group and keeps a copy of what is said in it.
--
-- Never returns and never throws a synchronous exception: a failure of one
-- poll is logged and the next poll follows after
-- 'retryDelayMicroseconds'.
--
-- Takes the token and the group rather than reading them from the config,
-- so a deployment without a telegram group does not start this thread at
-- all rather than starting one, which has nothing to follow.
runTelegramMirror
  :: MonadIO m
  => TelegramBotToken
  -> Int64
  -> AppT m ()
runTelegramMirror token chatId' = do
  state <- ask
  liftIO $ do
    runAppT state $ runLogging $ $(logInfo)
      ( "runTelegramMirror: following telegram group " <> tshow chatId' )
    lastCheckedV <- newIORef Nothing
    lastSweptV <- newIORef Nothing
    forever $ do
      -- around all three, not only the poll: the process ends when this
      -- thread does, so anything, which escapes any of them, takes the
      -- service with it. The calls inside contain their own failures
      -- already, which leaves this for what nothing else expected
      eresult <- E.tryAny $ do
        checkStillInGroup state lastCheckedV token chatId'
        reconcileDeletions state lastSweptV token chatId'
        runAppT state $ pollOnce token chatId'
      case eresult of
        -- nothing to do either way: telegram either had updates, or held
        -- the poll open for its timeout and had none
        Right (Right _updatesRead) -> return ()
        Right (Left err) -> do
          runAppT state $ runLogging $ $(logWarn)
            ( "runTelegramMirror: " <> describeError err
            <> ", retrying in " <> tshow (retryDelayMicroseconds `div` 1000000)
            <> " seconds"
            )
          threadDelay retryDelayMicroseconds
        Left (err :: E.SomeException) -> do
          runAppT state $ runLogging $ $(logWarn)
            ( "runTelegramMirror: unexpected failure: " <> tshow err
            <> ", retrying in " <> tshow (retryDelayMicroseconds `div` 1000000)
            <> " seconds"
            )
          threadDelay retryDelayMicroseconds

-- | reads the group once and stores whatever it said, returning how many
-- updates were read.
--
-- Telegram holds the request open while it waits, so this takes as long as
-- the configured poll timeout when the group is quiet, and returns at once
-- when it is not.
pollOnce
  :: ( MonadIO m
     , MonadMonitor m
     )
  => TelegramBotToken
  -> Int64
  -> AppT m (Either CallstackError Int)
pollOnce token chatId' = do
  State{ config = Config
           { configTelegramAPIURL = apiURL
           , configTelegramPollTimeoutSecs = timeoutSecs
           }
       } <- ask
  mstored <- withDBTransaction "V1.TelegramClient.pollOnce.offset"
    $ getPollOffsetTx chatId'
  case mstored of
    Nothing -> return $! Left dbQueryError
    Just mlastUpdateId -> do
      let request = defaultGetUpdatesRequest
            { getUpdatesOffset = fmap (+ 1) mlastUpdateId
            , getUpdatesTimeout = fromPositive timeoutSecs
            }
      eresponse <- liftIO $ withTelegram apiURL
        $ getUpdates (BotPath (unTelegramBotToken token)) request
      case eresponse >>= resultOf of
        Left err -> return $! Left err
        -- telegram held the poll open and had nothing. The offset does
        -- not move, but the answer is recorded, so a mirror, which has
        -- stopped, does not look like a quiet group
        Right [] -> do
          now <- getCurrentTimeDB
          _ <- withDBTransaction "V1.TelegramClient.pollOnce.touch"
            $ touchPollStateTx chatId' now
          return $! Right 0
        Right updates -> do
          now <- getCurrentTimeDB
          let mirrored = mapMaybe (mirroredOf chatId' now) updates
              -- somebody pressing Start on the link the website gave them.
              -- These arrive in a one to one conversation with the bot, not
              -- in the group, so they are never part of the feed
              connectAttempts = mapMaybe connectAttemptOf updates
              -- every update read is covered, including the ones this
              -- service does not follow: leaving a gap would have telegram
              -- send them again on every poll, forever
              newOffset = maximum (map updateId updates)
          mwritten <- withDBTransaction "V1.TelegramClient.pollOnce.store" $ do
            written <- mapM store mirrored
            connected <- mapM (redeem now) connectAttempts
            advancePollStateTx chatId' newOffset now
            return $! (catMaybes written, connected)
          case mwritten of
            Nothing -> return $! Left dbQueryError
            Just (stored, connected) -> do
              mapM_ (tell token apiURL) connected
              -- after the transaction, never inside it: a connection told
              -- about a message, which the transaction then failed to
              -- write, would be told about something nobody can read
              --
              -- published without withLiveEventOrder, which serialises
              -- changes of offers, contracts and balances so a connection
              -- never sees an older one after a newer. The chat is none of
              -- those and orders itself by the group's own message ids, and
              -- taking that lock would make a slow poll hold up accepting
              -- an offer
              mapM_ announce stored
              return $! Right (length updates)
  where
    redeem now (privateChatId, senderId', code) = do
      ok <- redeemLinkCodeTx code senderId' now
      return (privateChatId, ok)
    -- pressing Start and being told nothing at all is indistinguishable
    -- from pressing it and it not having worked, so the bot says which
    tell token' apiURL (privateChatId, ok) = liftIO $ do
      _ <- withTelegram apiURL $ sendMessage
        (BotPath (unTelegramBotToken token'))
        ( SendMessageRequest privateChatId
          $ if ok
            then "Your Telegram is now connected to your Op Energy account."
            else "That link has already been used or has expired. Open your\
                 \ profile on the website and press Connect Telegram again."
        )
      return ()
    -- only what was actually written is announced: a redelivery, which the
    -- unique constraint turned into nothing, must not be announced twice
    announce (row, wasEdit) = publishLiveEvent $! LiveEvent
      { liveEventMessage =
          ( if wasEdit then LiveMessageTelegramEdited else LiveMessageTelegramMessage
          ) $! telegramMessageInfoOf Nothing row
      , liveEventBalances = []
      }
    store message
      | mirroredIsEdit message =
          let row = mirroredRow message
          in do
            changed <- updateTelegramMessageBodyTx
              (telegramGroupMessageChatId row)
              (telegramGroupMessageMessageId row)
              (telegramGroupMessageBody row)
              -- the time of the edit, not the time the message was sent:
              -- an edit carries both, and storing the send time here would
              -- have every edited message claim it was edited at the moment
              -- it was written. The fallback is the time this poll read it,
              -- which is the closest thing we have if telegram ever sends an
              -- edit with no edit_date
              ( fromMaybe (telegramGroupMessageCreatedAt row)
                  (telegramGroupMessageEditedAt row)
              )
            -- an edit of a message, which is not mirrored or has been
            -- removed, changes nothing, so there is nothing to announce
            return $! if changed then Just (row, True) else Nothing
      | otherwise = do
          wasNew <- insertTelegramMessageTx (mirroredRow message)
          return $! if wasNew then Just (mirroredRow message, False) else Nothing

-- | the row one update is to be stored as, or Nothing where the update is
-- not part of the chat.
--
-- Ignored: an update from another chat, a message from a channel or from a
-- one to one conversation with the bot, and a message with no text at all,
-- which is a photo or a sticker and which this feed has nothing to show
-- for.
mirroredOf :: Int64 -> UTCTime -> Update -> Maybe MirroredMessage
mirroredOf chatId' now update = case (updateMessage update, updateEditedMessage update) of
  (Just message, _) -> fmap (\row -> MirroredMessage row False) (rowOf message)
  (_, Just edited) -> fmap (\row -> MirroredMessage row True) (rowOf edited)
  _ -> Nothing
  where
    rowOf message = do
      body <- messageText message
      _ <- if isGroupMessage message then Just () else Nothing
      _ <- if chatId (messageChat message) == chatId' then Just () else Nothing
      return $! TelegramGroupMessage
        { telegramGroupMessageChatId = chatId (messageChat message)
        , telegramGroupMessageMessageId = messageId message
        , telegramGroupMessageUpdateId = updateId update
        , telegramGroupMessageSenderId = fmap userId (messageFrom message)
        , telegramGroupMessageSenderUsername =
            messageFrom message >>= userUsername
        , telegramGroupMessageSenderDisplayName = clampName
            (senderDisplayNameOf message)
        , telegramGroupMessageBody = body
        , telegramGroupMessageSentAt = telegramTime (messageDate message)
        , telegramGroupMessageEditedAt =
            fmap telegramTime (messageEditDate message)
        , telegramGroupMessageDeletedAt = Nothing
        , telegramGroupMessageDeletedByUUID = Nothing
        , telegramGroupMessageCreatedAt = now
        }

-- | asks telegram, now and then, which of the recent messages are still
-- in the group, and hides the ones, which are not.
--
-- Telegram sends a bot nothing at all when a message is deleted, so without
-- this the feed shows a removed message for ever. A moderator, who removes
-- something from our site, takes it out of both at once -- this is for
-- everything removed in the telegram app instead, which is where a group's
-- own moderators will do it.
--
-- Nothing is hidden on anything but a definite answer. An unreachable
-- telegram, or a refusal this does not recognise, leaves every message
-- exactly as it is: showing a message, which has been removed, is a
-- mistake, which the next sweep corrects, while hiding one, which is
-- really there, is a mistake nothing corrects.
--
-- A failure here is logged and nothing else, for the same reason as in
-- 'checkStillInGroup': the mirror has to go on polling either way.
reconcileDeletions
  :: State
  -> IORef (Maybe UTCTime)
  -> TelegramBotToken
  -> Int64
  -> IO ()
reconcileDeletions state lastSweptV token chatId' = do
  now <- getCurrentTime
  mlastSwept <- readIORef lastSweptV
  let due = case mlastSwept of
        Nothing -> True
        Just lastSwept ->
          realToFrac (diffUTCTime now lastSwept) > reconcileSeconds
  when due $ do
    writeIORef lastSweptV (Just now)
    State{ config = Config{ configTelegramAPIURL = apiURL } } <- return state
    mrows <- runAppT state
      $ withDBTransaction "V1.TelegramClient.reconcileDeletions"
      $ messagesToReconcileTx chatId' reconcileBatch
    case mrows of
      Nothing -> runAppT state $ runLogging $ $(logWarn)
        "reconcileDeletions: the messages to sweep could not be read"
      Just rows -> sweep apiURL reconcileGoneLimit rows
  where
    -- stops the sweep once it has been told that too many messages in a
    -- row are gone. Telegram answering that about message after message is
    -- not what a group being moderated looks like, it is what a bot, which
    -- can no longer see the group, looks like -- and hiding a message
    -- cannot be undone, so the sweep gives up rather than carry on being
    -- sure
    sweep _ _ [] = return ()
    sweep apiURL budget (row : rest)
      | budget <= (0 :: Int) = runAppT state $ runLogging $ $(logWarn)
          ( "reconcileDeletions: telegram said " <> tshow reconcileGoneLimit
          <> " messages in a row were gone, which is more like the bot"
          <> " having lost sight of the group than like moderation, so"
          <> " this sweep stopped rather than hide any more"
          )
      | otherwise = do
          let messageId' = telegramGroupMessageMessageId row
          presence <- messageStillThere apiURL
            (BotPath (unTelegramBotToken token)) chatId' messageId'
          budget' <- case presence of
            MessageStillThere -> return reconcileGoneLimit -- budget restored
            -- said without being sure, so nothing is done and the next
            -- sweep asks again
            MessagePresenceUnknown _ -> return budget
            MessageGone -> do
              hide messageId'
              return $! budget - 1
          threadDelay reconcileSpacingMicroseconds
          sweep apiURL budget' rest
    hide messageId' = do
      -- the DB's clock, not this process's: a stored time has to agree
      -- with what a later read of it returns
      goneAt <- runAppT state getCurrentTimeDB
      mdone <- runAppT state
        $ withDBTransaction "V1.TelegramClient.reconcileDeletions.hide"
        $ markTelegramMessageGoneTx chatId' messageId' goneAt
      case mdone of
        Nothing -> runAppT state $ runLogging $ $(logWarn)
          ( "reconcileDeletions: message " <> tshow messageId'
          <> " has gone from the group but could not be marked as removed"
          )
        Just () -> do
          runAppT state $ publishLiveEvent $! LiveEvent
            { liveEventMessage =
                LiveMessageTelegramDeleted (TelegramMessageID (tshow messageId'))
            , liveEventBalances = []
            }
          runAppT state $ runLogging $ $(logInfo)
            ( "reconcileDeletions: message " <> tshow messageId'
            <> " was removed in telegram and is no longer shown"
            )

-- | asks telegram, now and then, whether the bot is still in the group.
--
-- Nothing else notices this: being removed from a group does not make
-- getUpdates fail, it makes it answer "nothing new" for ever, which is
-- indistinguishable from a quiet group. So the feed would simply stop
-- filling and nothing would say why.
--
-- A failure here is logged and nothing else: the mirror goes on polling,
-- because telegram being briefly unreachable is not the same as the bot
-- having been thrown out, and this check cannot tell the two apart.
checkStillInGroup
  :: State
  -> IORef (Maybe UTCTime)
  -> TelegramBotToken
  -> Int64
  -> IO ()
checkStillInGroup state lastCheckedV token chatId' = do
  now <- getCurrentTime
  mlastChecked <- readIORef lastCheckedV
  let due = case mlastChecked of
        Nothing -> True
        Just lastChecked ->
          realToFrac (diffUTCTime now lastChecked) > membershipCheckSeconds
  when due $ do
    writeIORef lastCheckedV (Just now)
    State{ config = Config{ configTelegramAPIURL = apiURL } } <- return state
    -- what the bot's own standing in the group is, rather than whether the
    -- group can be described: getChat still answers for a group, which the
    -- bot has been demoted in, so it cannot tell that moderation has
    -- stopped working
    case botUserIdOf (unTelegramBotToken token) of
      Nothing -> runAppT state $ runLogging $ $(logWarn)
        "checkStillInGroup: the configured token is not shaped like a bot token"
      Just botUserId -> do
        eresponse <- withTelegram apiURL
          $ getChatMember (BotPath (unTelegramBotToken token))
              (ChatMemberRequest chatId' botUserId)
        case eresponse >>= resultOf of
          Left err -> runAppT state $ runLogging $ $(logWarn)
            ( "checkStillInGroup: telegram will not say what this bot is in"
            <> " group " <> tshow chatId' <> " (" <> describeError err
            <> "). It may have been removed from the group, in which case the"
            <> " trade chat has stopped filling and nothing else will say so"
            )
          Right member -> reportStanding member

  where
    -- being in the group is not enough: a bot, which is no longer an
    -- administrator, goes on mirroring perfectly well while every delete
    -- and every ban a moderator asks for is refused, and nothing else
    -- anywhere says why
    reportStanding member
      | chatMemberStatus member /= "administrator" =
          runAppT state $ runLogging $ $(logWarn)
            ( "checkStillInGroup: this bot is \"" <> chatMemberStatus member
            <> "\" in group " <> tshow chatId' <> " rather than an"
            <> " administrator, so a moderator cannot remove a message or a"
            <> " person from the website"
            )
      | chatMemberCanDeleteMessages member /= Just True =
          runAppT state $ runLogging $ $(logWarn)
            ( "checkStillInGroup: this bot may not delete messages in group "
            <> tshow chatId' <> ", so a moderator's removal will be refused"
            )
      | chatMemberCanRestrictMembers member /= Just True =
          runAppT state $ runLogging $ $(logWarn)
            ( "checkStillInGroup: this bot may not restrict members of group "
            <> tshow chatId' <> ", so a moderator's ban will be refused"
            )
      | otherwise = return ()

-- | the connect code somebody sent the bot, where this update is one.
--
-- The website hands out a link, which opens the bot with a code in it, and
-- pressing Start sends that code as "/start <code>" from the telegram
-- account, which pressed it. So the two accounts are proved to be the same
-- person by the pressing, and no name has to be typed or trusted.
connectAttemptOf :: Update -> Maybe (Int64, Int64, Text)
connectAttemptOf update = do
  message <- updateMessage update
  _ <- if isPrivateMessage message then Just () else Nothing
  sender <- messageFrom message
  body <- messageText message
  code <- Text.stripPrefix "/start " (Text.strip body)
  let trimmed = Text.strip code
  _ <- if Text.null trimmed then Nothing else Just ()
  return (chatId (messageChat message), userId sender, trimmed)

-- | telegram's seconds since the epoch, as a time this service can store
telegramTime :: Int64 -> UTCTime
telegramTime = posixSecondsToUTCTime . fromIntegral

-- | a sender's name is theirs to choose, so it is cut to one line and to a
-- length, which cannot push the rest of a feed row off the screen
clampName :: Text -> Text
clampName = Text.take 64 . Text.takeWhile (\c -> c /= '\n' && c /= '\r')
