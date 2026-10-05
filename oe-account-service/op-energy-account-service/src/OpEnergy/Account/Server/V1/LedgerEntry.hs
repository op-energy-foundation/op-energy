{-- | This module defines the LedgerEntry entity: one row for every change of
 - an account's balance, with the reason for it.
 -
 - 'adjustBalanceTx' is the only procedure, which changes Person.balance: it
 - moves the balance and appends the matching entry in one transaction, or
 - does neither. So a balance is always the sum of its entries, and every
 - change can be explained to the account's owner.
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
module OpEnergy.Account.Server.V1.LedgerEntry
  where

import           Control.Monad (when)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Except (runExceptT, throwE)
import           Control.Monad.Trans.Reader (ReaderT)
import           Data.Int (Int64)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time.Clock.POSIX (POSIXTime)
import           GHC.Generics

import           Database.Persist
import           Database.Persist.Sql
                 ( PersistFieldSql(..), SqlBackend
                 , updateWhereCount
                 )
import           Database.Persist.TH

import           Data.OpEnergy.Account.API.V1.Sats (Sats(..))
import           Data.OpEnergy.Account.API.V2.LedgerReason
                 ( LedgerReason(..)
                 , ledgerReasonToText
                 , ledgerReasonFromText
                 )

import           OpEnergy.Account.Server.V1.Person
import           OpEnergy.Error
                 ( CallstackError, accountNotFound, insufficientBalance
                 )
import           OpEnergy.ExceptMaybe (exceptTMaybeT)

-- | which way a balance moved. Amounts are stored as a positive magnitude,
-- as Sats is unsigned, so the direction is what makes an entry a credit or
-- a debit
data LedgerDirection
  = Credit
  | Debit
  deriving (Show, Eq, Ord, Generic, Enum, Bounded)

-- | lowercase serialisation, so the stored value stays readable in psql
ledgerDirectionToText :: LedgerDirection -> Text
ledgerDirectionToText Credit = "credit"
ledgerDirectionToText Debit  = "debit"

ledgerDirectionFromText :: Text -> Either Text LedgerDirection
ledgerDirectionFromText "credit" = Right Credit
ledgerDirectionFromText "debit"  = Right Debit
ledgerDirectionFromText other    =
  Left $ "LedgerDirection: unknown direction: " <> other

share [mkPersist sqlSettings, mkMigrate "migrateLedgerEntry"] [persistLowerCase|
LedgerEntry
  personId PersonId -- local relation, as the row is always written where the key is known
  direction LedgerDirection
  amountSats Sats -- positive magnitude: 'direction' says which way it moved
  reason LedgerReason
  balanceAfter Sats -- the balance this entry produced, read in the same transaction
  reference Text Maybe -- what the change relates to, eg "offer:42" or "contract:17"
  createdAt POSIXTime
  deriving Eq Show Generic
|]

-- PersistField instances of the API enums live in the service layer, as API
-- types should carry no Persist instances -- same convention as Sats and
-- HashedPassword in OpEnergy.Account.Server.V1.Person
instance PersistField LedgerReason where
  toPersistValue = toPersistValue . ledgerReasonToText
  fromPersistValue v = fromPersistValue v >>= ledgerReasonFromText
instance PersistFieldSql LedgerReason where
  sqlType _ = SqlString

instance PersistField LedgerDirection where
  toPersistValue = toPersistValue . ledgerDirectionToText
  fromPersistValue v = fromPersistValue v >>= ledgerDirectionFromText
instance PersistFieldSql LedgerDirection where
  sqlType _ = SqlString

-- | moves the given account's balance and records why, in one transaction:
-- either the balance changes and the entry exists, or neither does.
--
-- A debit is guarded with @PersonBalance >=.@, so the balance can not
-- underflow (Sats is unsigned, so subtracting too much would wrap around).
-- The balance is read back in the same transaction, which holds the
-- account's row lock, so no other change can come in between.
--
-- Example:
--
-- > adjustBalanceTx key Debit (Sats 5000) Stake (Just "offer:42") now
adjustBalanceTx
  :: MonadIO m
  => PersonId
  -> LedgerDirection
  -> Sats
  -> LedgerReason
  -> Maybe Text
  -> POSIXTime
  -> ReaderT SqlBackend m (Either CallstackError Sats)
adjustBalanceTx key direction amountSats reason reference now = runExceptT $ do
  changed <- lift $ case direction of
    Debit -> updateWhereCount
      [ PersonId ==. key, PersonBalance >=. amountSats ]
      [ PersonBalance -=. amountSats
      , PersonLastUpdated =. now
      ]
    Credit -> updateWhereCount
      [ PersonId ==. key ]
      [ PersonBalance +=. amountSats
      , PersonLastUpdated =. now
      ]
  when (changed /= (1 :: Int64)) $ throwE $ case direction of
    Debit -> insufficientBalance
    Credit -> accountNotFound
  balanceAfter <- exceptTMaybeT accountNotFound $ fmap personBalance <$> get key
  _ <- lift $ insert $ LedgerEntry
    { ledgerEntryPersonId = key
    , ledgerEntryDirection = direction
    , ledgerEntryAmountSats = amountSats
    , ledgerEntryReason = reason
    , ledgerEntryBalanceAfter = balanceAfter
    , ledgerEntryReference = reference
    , ledgerEntryCreatedAt = now
    }
  return balanceAfter

-- | reference of an entry, which relates to a record of another service,
-- eg @ledgerReference "offer" "42"@ is @"offer:42"@
ledgerReference :: Text -> Text -> Text
ledgerReference kind recordId = kind <> ":" <> recordId

-- | 'ledgerReference' of a record, which is identified by a number
ledgerReferenceOfKey :: Text -> Int64 -> Text
ledgerReferenceOfKey kind key =
  ledgerReference kind (Text.pack (show key))
