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
import           Control.Monad (forever)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Logger (logInfo, logWarn)
import           Control.Monad.Trans.Reader (ask)
import qualified Control.Exception.Safe as E
import           Data.Int (Int64)
import           Data.Maybe (catMaybes, mapMaybe)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time.Clock (UTCTime)
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
                 , GetUpdatesRequest(..)
                 , Message(..)
                 , TelegramChat(..)
                 , TelegramUser(..)
                 , Update(..)
                 , defaultGetUpdatesRequest
                 , getUpdates
                 , isGroupMessage
                 , resultOf
                 , senderDisplayNameOf
                 , withTelegram
                 )
import           OpEnergy.Offer.Server.V1.LiveEvent (LiveEvent(..))
import           OpEnergy.Offer.Server.V1.Time (getCurrentTimeDB)
import           OpEnergy.Offer.Server.V1.WebSocketService (publishLiveEvent)
import           Data.OpEnergy.Offer.API.V1.LiveMessage (LiveMessage(..))
import           OpEnergy.Offer.Server.V1.TelegramMessage
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
    forever $ do
      -- the inner call contains every telegram and DB failure already, so
      -- this catches only what nothing else expected. It is still here
      -- because the process ends when this thread does
      eresult <- E.tryAny $ runAppT state $ pollOnce token chatId'
      case eresult of
        Right (Right updatesRead)
          | updatesRead > 0 -> return () -- straight round again
          | otherwise -> return () -- telegram held the poll open and had nothing
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
        Right [] -> return $! Right 0
        Right updates -> do
          now <- getCurrentTimeDB
          let mirrored = mapMaybe (mirroredOf chatId' now) updates
              -- every update read is covered, including the ones this
              -- service does not follow: leaving a gap would have telegram
              -- send them again on every poll, forever
              newOffset = maximum (map updateId updates)
          mwritten <- withDBTransaction "V1.TelegramClient.pollOnce.store" $ do
            written <- mapM store mirrored
            advancePollStateTx chatId' newOffset now
            return $! catMaybes written
          case mwritten of
            Nothing -> return $! Left dbQueryError
            Just stored -> do
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
            updateTelegramMessageBodyTx
              (telegramGroupMessageChatId row)
              (telegramGroupMessageMessageId row)
              (telegramGroupMessageBody row)
              (telegramGroupMessageSentAt row)
            return $! Just (row, True)
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

-- | telegram's seconds since the epoch, as a time this service can store
telegramTime :: Int64 -> UTCTime
telegramTime = posixSecondsToUTCTime . fromIntegral

-- | a sender's name is theirs to choose, so it is cut to one line and to a
-- length, which cannot push the rest of a feed row off the screen
clampName :: Text -> Text
clampName = Text.take 64 . Text.takeWhile (\c -> c /= '\n' && c /= '\r')
