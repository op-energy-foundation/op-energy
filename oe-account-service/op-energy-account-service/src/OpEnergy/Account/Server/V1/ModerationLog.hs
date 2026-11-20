{-- | This module defines the ModerationLog entity: one row for every
 - action a moderator takes in this service, with who took it and against
 - what.
 -
 - The offer service keeps a log of its own, with the same shape, because
 - the two services have separate databases and neither can write to the
 - other's. So this table holds the actions this service performs -- bans
 - -- and the offer service's table holds the ones it performs, such as the
 - removal of a chat message.
 -
 - A row is written in the same transaction as the action it records, so
 - there can be no ban without its entry and no entry without its ban.
 -}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE EmptyDataDecls             #-}
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
module OpEnergy.Account.Server.V1.ModerationLog
  where

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Reader (ReaderT)
import           Data.Text (Text)
import           Data.Time.Clock.POSIX (POSIXTime)
import           GHC.Generics

import           Database.Persist
import           Database.Persist.Sql (PersistFieldSql(..), SqlBackend)
import           Database.Persist.TH

import qualified Data.OpEnergy.Account.API.V1.UUID as API

import           OpEnergy.Account.Server.V1.Person

-- | what a moderator did. Only the actions this service performs: the
-- removal of a chat message is the offer service's to record, as that is
-- where the message lives
data ModerationAction
  = BanAccount
  deriving (Show, Eq, Ord, Generic, Enum, Bounded)

-- | lowercase serialisation, so the stored value stays readable in psql
moderationActionToText :: ModerationAction -> Text
moderationActionToText BanAccount = "ban_account"

moderationActionFromText :: Text -> Either Text ModerationAction
moderationActionFromText "ban_account" = Right BanAccount
moderationActionFromText other =
  Left $ "ModerationAction: unknown action: " <> other

-- | what the action was taken against. Kept beside the id because the id
-- of an account and the id of a message are not the same kind of thing,
-- and a reader of the log has to know which one a row names
data ModerationTarget
  = AccountTarget
  deriving (Show, Eq, Ord, Generic, Enum, Bounded)

moderationTargetToText :: ModerationTarget -> Text
moderationTargetToText AccountTarget = "account"

moderationTargetFromText :: Text -> Either Text ModerationTarget
moderationTargetFromText "account" = Right AccountTarget
moderationTargetFromText other =
  Left $ "ModerationTarget: unknown target: " <> other

share [mkPersist sqlSettings, mkMigrate "migrateModerationLog"] [persistLowerCase|
ModerationLog
  moderatorUUID (API.UUID Person) -- who acted. Held as the cross-service UUID rather than as a local key, so a row still names its moderator if that account is ever archived
  action ModerationAction
  target ModerationTarget
  targetId Text -- which one, as text: the ids of the things a moderator acts on are not of one type
  details Text Maybe -- anything worth reading later, such as the display name at the time
  createdAt POSIXTime
  deriving Eq Show Generic
|]

-- PersistField instances of the log's enums, beside the entity they belong
-- to -- same convention as LedgerDirection in LedgerEntry
instance PersistField ModerationAction where
  toPersistValue = toPersistValue . moderationActionToText
  fromPersistValue v = fromPersistValue v >>= moderationActionFromText
instance PersistFieldSql ModerationAction where
  sqlType _ = SqlString

instance PersistField ModerationTarget where
  toPersistValue = toPersistValue . moderationTargetToText
  fromPersistValue v = fromPersistValue v >>= moderationTargetFromText
instance PersistFieldSql ModerationTarget where
  sqlType _ = SqlString

-- | appends one entry. Runs inside the caller's transaction, so the entry
-- and the action it records are written together or not at all.
--
-- Example:
--
-- > recordModerationTx moderatorUUID BanAccount AccountTarget uuidText
-- >   (Just displayName) now
recordModerationTx
  :: MonadIO m
  => API.UUID Person
  -> ModerationAction
  -> ModerationTarget
  -> Text
  -> Maybe Text
  -> POSIXTime
  -> ReaderT SqlBackend m ()
recordModerationTx moderatorUUID action target targetId details now = do
  _ <- insert $! ModerationLog
    { moderationLogModeratorUUID = moderatorUUID
    , moderationLogAction = action
    , moderationLogTarget = target
    , moderationLogTargetId = targetId
    , moderationLogDetails = details
    , moderationLogCreatedAt = now
    }
  return ()
