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

import           Database.Persist
import           Database.Persist.Sql (SqlBackend)
import           Database.Persist.TH

import           Data.OpEnergy.Offer.API.V1.TelegramMessageInfo
                 ( TelegramMessageID(..)
                 , TelegramMessageInfo(..)
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
  deletedByUUID Text Maybe -- the account of the moderator, who removed it
  createdAt UTCTime -- when this service read it, which is not when it was said
  UniqueTelegramGroupMessage chatId messageId
  deriving Eq Show Generic

-- | how far one group has been read. One row per group
TelegramPollState
  chatId Int64
  lastUpdateId Int64 -- the next poll asks telegram for this plus one, which is also what confirms to telegram that everything up to it arrived. Advanced in the same transaction as the rows it covers
  lastPolledAt UTCTime -- when telegram last answered, whether or not it had anything to say. How long ago this was is what tells a dead mirror from a quiet group
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
  :: Maybe Text
  -> TelegramGroupMessage
  -> TelegramMessageInfo
telegramMessageInfoOf mOpEnergyName row = TelegramMessageInfo
  { messageId = TelegramMessageID $! Text.pack $! show
      $! telegramGroupMessageMessageId row
  , senderName = telegramGroupMessageSenderDisplayName row
  , senderDisplayName = mOpEnergyName
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
  return $! maybe False (const True) mkey

-- | records an edit of a message already mirrored, leaving everything else
-- about the row alone.
--
-- Telegram sends an edit as a whole message rather than as a difference, so
-- what is stored is the new text and when it was edited.
updateTelegramMessageBodyTx
  :: MonadIO m
  => Int64
  -> Int64
  -> Text
  -> UTCTime
  -> ReaderT SqlBackend m ()
updateTelegramMessageBodyTx chatId' messageId' body' editedAt' =
  updateWhere
    [ TelegramGroupMessageChatId ==. chatId'
    , TelegramGroupMessageMessageId ==. messageId'
    ]
    [ TelegramGroupMessageBody =. body'
    , TelegramGroupMessageEditedAt =. Just editedAt'
    ]

-- | marks a message as removed by a moderator.
--
-- The row stays. Telegram never tells a bot that a message was deleted, so
-- a row deleted outright would be written again by the next redelivery, and
-- the unique constraint cannot catch that once the row is gone.
markTelegramMessageDeletedTx
  :: MonadIO m
  => Int64
  -> Int64
  -> Text
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

-- | where to carry on reading this group from, or Nothing when it has never
-- been read, which asks telegram for whatever it still holds.
getPollOffsetTx
  :: MonadIO m
  => Int64
  -> ReaderT SqlBackend m (Maybe Int64)
getPollOffsetTx chatId' = do
  mstate <- getBy (UniqueTelegramPollState chatId')
  return $! fmap (telegramPollStateLastUpdateId . entityVal) mstate
