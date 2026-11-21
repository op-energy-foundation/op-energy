{-- | This module defines the tables the trade chat is mirrored into.
 -
 - The chat itself lives in a telegram group; this service keeps a copy of
 - it so the website can show a feed of it. The copy is not a cache: a bot
 - cannot read anything said before it joined, and telegram throws away an
 - update it could not deliver after 24 hours, so these rows are the only
 - history of the chat, which will ever exist. Nothing here may be dropped
 - and refetched.
 -
 - 'TelegramPollState' remembers how far the group has been read. It is
 - advanced in the same transaction as the messages it covers, so a crash
 - between the two leaves telegram redelivering rather than skipping, and
 - 'UniqueTelegramGroupMessage' absorbs the redelivery.
 -}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
module OpEnergy.Offer.Server.V1.TelegramMessage
  where

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Reader (ReaderT)
import           Data.Int (Int64)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time.Clock (UTCTime)
import           GHC.Generics

import           Data.Maybe (isJust)
import           Database.Persist
import           Database.Persist.Sql (SqlBackend, updateWhereCount)
import           Database.Persist.TH

import qualified Data.OpEnergy.Account.API.V1.Account as AccountAPI
import qualified Data.OpEnergy.Account.API.V1.UUID as AccountAPI
import           Servant.API (toQueryParam)

import           Data.OpEnergy.Offer.API.V1.TelegramMessageID
                 ( TelegramMessageID(..)
                 )
import           Data.OpEnergy.Offer.API.V1.TelegramMessageInfo
                 ( TelegramMessageInfo(..)
                 )

share [mkPersist sqlSettings, mkMigrate "migrateTelegramMessage"] [persistLowerCase|
-- | one message of the telegram group, as it was when it was read
TelegramGroupMessage
  chatId Int64 -- the group this was said in. Telegram's group ids are negative
  messageId Int64 -- telegram's own id for it, unique within the group. The natural key here: an edit of a message arrives as a fresh update carrying the same message id, so a redelivery is matched on this rather than on updateId
  updateId Int64 -- the update this row was first written from, so a row can be traced back to a poll. Not unique: an edit brings a new one
  senderId Int64 Maybe -- telegram's id for whoever said it, which is what banning them takes. Absent when an anonymous administrator said it, as such a message carries the group instead of a sender, and there is then nobody to ban
  senderUsername Text Maybe -- their @name. Optional in telegram and absent for most accounts, so it is never what a sender is shown as or matched by
  senderDisplayName Text -- what the sender is shown as: their first name, which telegram always has, or the group's title for an anonymous administrator
  body Text -- what they said. A message without text -- a photo, a sticker -- is not stored at all
  sentAt UTCTime
  editedAt UTCTime Maybe -- when they last edited it, where they did
  deletedAt UTCTime Maybe -- when a moderator removed it. The row stays: telegram never tells a bot that a message was deleted, so a row deleted outright would be written again by the next redelivery, which the unique index cannot catch once the row is gone
  deletedByUUID (AccountAPI.UUID AccountAPI.Person) Maybe -- the account of the moderator, who removed it
  createdAt UTCTime -- when this service read it, which is not when it was said
  UniqueTelegramGroupMessage chatId messageId
  deriving Eq Show Generic

-- | how far one group has been read. One row per group
TelegramPollState
  chatId Int64
  lastUpdateId Int64 -- the next poll asks telegram for this plus one, which is also what confirms to telegram that everything up to it arrived. Advanced in the same transaction as the rows it covers
  lastPolledAt UTCTime -- when telegram last answered, whether or not it had anything to say, so how long ago this was tells a mirror, which has stopped, from a group, which is quiet
  UniqueTelegramPollState chatId
  deriving Eq Show Generic
|]

-- | the message as the website is given it.
--
-- The op energy name is passed in rather than read here, because it comes
-- from the link table and a feed renders a page of messages against one
-- read of it rather than one read per row.
--
-- Example:
--
-- > telegramMessageInfoOf (Just "blur_inject_439") row
telegramMessageInfoOf
  :: Maybe AccountAPI.DisplayName
  -> TelegramGroupMessage
  -> TelegramMessageInfo
telegramMessageInfoOf mOpEnergyName row = TelegramMessageInfo
  { messageId = TelegramMessageID $! Text.pack $! show
      $! telegramGroupMessageMessageId row
  , senderName = telegramGroupMessageSenderDisplayName row
  , senderDisplayName = fmap toQueryParam mOpEnergyName
  , senderId = telegramGroupMessageSenderId row
  , body = telegramGroupMessageBody row
  , sentAt = telegramGroupMessageSentAt row
  , editedAt = telegramGroupMessageEditedAt row
  }

-- | writes one message of the group, or does nothing if it is already
-- there.
--
-- Runs inside the caller's transaction, so the messages of one poll and the
-- offset, which covers them, are written together or not at all.
--
-- Returns whether the row was new, because a redelivery must not be
-- announced to the website a second time.
--
-- Example:
--
-- > wasNew <- insertTelegramMessageTx message
insertTelegramMessageTx
  :: MonadIO m
  => TelegramGroupMessage
  -> ReaderT SqlBackend m Bool
insertTelegramMessageTx message = do
  mkey <- insertUnique message
  return $! isJust mkey

-- | rewrites a message, which was edited in the group, saying whether
-- there was one to rewrite.
--
-- Telegram sends an edit as a whole message rather than as a difference, so
-- what is stored is the new text and when it was edited.
--
-- Two rows are deliberately left alone. One, which was never mirrored,
-- because telegram replays an edit of a message, which this service never
-- saw -- anything said before the bot joined, or while it was down longer
-- than a redelivery covers -- and there is nothing to attach the new text
-- to. One, which has been removed, because an edit telegram had already
-- buffered when a moderator removed the message would otherwise write the
-- text back and announce it, and a reader would watch a removed message
-- return.
--
-- The caller needs to know which happened, as an edit, which changed
-- nothing, must not be announced to anybody.
updateTelegramMessageBodyTx
  :: MonadIO m
  => Int64
  -> Int64
  -> Text
  -> UTCTime
  -> ReaderT SqlBackend m Bool
updateTelegramMessageBodyTx chatId' messageId' body' editedAt' = do
  changed <- updateWhereCount
    [ TelegramGroupMessageChatId ==. chatId'
    , TelegramGroupMessageMessageId ==. messageId'
    , TelegramGroupMessageDeletedAt ==. Nothing
    ]
    [ TelegramGroupMessageBody =. body'
    , TelegramGroupMessageEditedAt =. Just editedAt'
    ]
  return $! changed > 0

-- | the newest messages of a group, which this service still believes are
-- there, newest first.
--
-- What the presence sweep asks telegram about. Only the newest are worth
-- asking about: the feed shows the recent end of the group, a sweep of the
-- whole history would grow without limit, and telegram rate limits a bot
-- whatever it is asking.
--
-- Example:
--
-- > toReconcile <- messagesToReconcileTx chatId 50
messagesToReconcileTx
  :: MonadIO m
  => Int64
  -> Int
  -> ReaderT SqlBackend m [TelegramGroupMessage]
messagesToReconcileTx chatId' howMany =
  fmap (map entityVal) $ selectList
    [ TelegramGroupMessageChatId ==. chatId'
    , TelegramGroupMessageDeletedAt ==. Nothing
    ]
    [ Desc TelegramGroupMessageMessageId
    , LimitTo howMany
    ]

-- | marks a message, which has gone from the group, as removed.
--
-- Separate from 'markTelegramMessageDeletedTx' because no moderator did
-- this: somebody removed it in telegram itself, and the row is left with no
-- moderator against it to say so. The row stays for the same reason as any
-- other removal -- telegram can redeliver an update, and a row deleted
-- outright would simply be written again.
markTelegramMessageGoneTx
  :: MonadIO m
  => Int64
  -> Int64
  -> UTCTime
  -> ReaderT SqlBackend m ()
markTelegramMessageGoneTx chatId' messageId' now =
  updateWhere
    [ TelegramGroupMessageChatId ==. chatId'
    , TelegramGroupMessageMessageId ==. messageId'
    , TelegramGroupMessageDeletedAt ==. Nothing
    ]
    [ TelegramGroupMessageDeletedAt =. Just now ]

-- | marks a message as removed by a moderator.
--
-- The row stays. Telegram never tells a bot that a message was deleted, so
-- a row deleted outright would be written again by the next redelivery, and
-- the unique constraint cannot catch that once the row is gone.
markTelegramMessageDeletedTx
  :: MonadIO m
  => Int64
  -> Int64
  -> AccountAPI.UUID AccountAPI.Person
  -> UTCTime
  -> ReaderT SqlBackend m ()
markTelegramMessageDeletedTx chatId' messageId' moderatorUUID now =
  updateWhere
    [ TelegramGroupMessageChatId ==. chatId'
    , TelegramGroupMessageMessageId ==. messageId'
    ]
    [ TelegramGroupMessageDeletedAt =. Just now
    , TelegramGroupMessageDeletedByUUID =. Just moderatorUUID
    ]

-- | moves the offset of one group to cover everything just written.
--
-- Written as an upsert, as the first poll of a group has no row to update
-- and every later one has.
advancePollStateTx
  :: MonadIO m
  => Int64
  -> Int64
  -> UTCTime
  -> ReaderT SqlBackend m ()
advancePollStateTx chatId' lastUpdateId' now =
  upsertBy
    (UniqueTelegramPollState chatId')
    (TelegramPollState chatId' lastUpdateId' now)
    [ TelegramPollStateLastUpdateId =. lastUpdateId'
    , TelegramPollStateLastPolledAt =. now
    ]
    >> return ()

-- | records that telegram answered, without moving the offset.
--
-- A quiet group answers nothing for as long as it is quiet, and a mirror,
-- which has silently stopped, answers nothing for ever. Without this the
-- two look identical in the database, and lastPolledAt would be no more
-- than the time of the last message -- which is the thing it is supposed
-- to be distinguishable from.
touchPollStateTx
  :: MonadIO m
  => Int64
  -> UTCTime
  -> ReaderT SqlBackend m ()
touchPollStateTx chatId' now =
  updateWhere
    [ TelegramPollStateChatId ==. chatId' ]
    [ TelegramPollStateLastPolledAt =. now ]

-- | where to carry on reading this group from, or Nothing when it has never
-- been read, which asks telegram for whatever it still holds.
getPollOffsetTx
  :: MonadIO m
  => Int64
  -> ReaderT SqlBackend m (Maybe Int64)
getPollOffsetTx chatId' = do
  mstate <- getBy (UniqueTelegramPollState chatId')
  return $! fmap (telegramPollStateLastUpdateId . entityVal) mstate
