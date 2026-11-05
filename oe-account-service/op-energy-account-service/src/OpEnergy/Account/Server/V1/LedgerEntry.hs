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

import           Control.Monad (unless, when)
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
                 , rawExecuteCount
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
                 , ledgerReasonMismatch
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

-- | which of an account's two pockets a movement belongs to.
--
-- 'FreeSats' are sats the account was given: they can be staked, and they
-- can be won and lost, but they can never be withdrawn. 'RealSats' are sats
-- somebody paid in, or won on top of a stake, and those can.
--
-- One movement belongs to one pocket: a stake is paid for with free sats or
-- with real sats, never with a mix of both, so a payout or a refund can be
-- returned to the pocket it came out of.
data FundsKind
  = FreeSats
  | RealSats
  deriving (Show, Eq, Ord, Generic, Enum, Bounded)

-- | lowercase serialisation, so the stored value stays readable in psql
fundsKindToText :: FundsKind -> Text
fundsKindToText FreeSats = "free"
fundsKindToText RealSats = "real"

fundsKindFromText :: Text -> Either Text FundsKind
fundsKindFromText "free" = Right FreeSats
fundsKindFromText "real" = Right RealSats
fundsKindFromText other  =
  Left $ "FundsKind: unknown kind: " <> other

share [mkPersist sqlSettings, mkMigrate "migrateLedgerEntry"] [persistLowerCase|
LedgerEntry
  personId PersonId -- local relation, as the row is always written where the key is known
  direction LedgerDirection
  amountSats Sats -- positive magnitude: 'direction' says which way it moved
  fundsKind FundsKind default='real' -- which pocket moved: free sats, which can never be withdrawn, or real ones. Rows written before the two pockets existed are real, as that is what they were
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

instance PersistField FundsKind where
  toPersistValue = toPersistValue . fundsKindToText
  fromPersistValue v = fromPersistValue v >>= fundsKindFromText
instance PersistFieldSql FundsKind where
  sqlType _ = SqlString

-- | moves the given account's balance and records why, in one transaction:
-- either the balance changes and the entry exists, or neither does.
--
-- The 'FundsKind' says which pocket moves. A debit of 'RealSats' is the only
-- movement, which can leave the service, so it is guarded with
-- @balance - free_sats >= amount@: free sats can not be withdrawn, whatever
-- a caller asks for. A debit of 'FreeSats' is guarded with
-- @free_sats >= amount@, so a stake, which says it is paid for with free
-- sats, is only taken when the account holds that many.
--
-- Both guards sit in the statement, which moves the balance, so a refused
-- movement changes nothing and the balance can not underflow (Sats is
-- unsigned, so subtracting too much would wrap around). The balance is read
-- back in the same transaction, which holds the account's row lock, so no
-- other change can come in between.
--
-- Example:
--
-- > adjustBalanceTx key Debit (Sats 5000) FreeSats Stake (Just "offer:42") now
adjustBalanceTx
  :: MonadIO m
  => PersonId
  -> LedgerDirection
  -> Sats
  -> FundsKind
  -> LedgerReason
  -> Maybe Text
  -> POSIXTime
  -> ReaderT SqlBackend m (Either CallstackError Sats)
adjustBalanceTx key direction amountSats funds reason reference now =
    runExceptT $ do
  unless (reasonDescribes direction reason) $ throwE ledgerReasonMismatch
  changed <- lift $ case (direction, funds) of
    -- Persistent's filters compare a column with a value and never with
    -- another column, so the guard, which keeps free sats out of a
    -- withdrawal, is written out in SQL. A rename of either column makes
    -- this statement fail loudly instead of passing quietly, and a
    -- statement, which fails, refuses the payment
    (Debit, RealSats) -> rawExecuteCount
      "UPDATE person SET balance = balance - ?, last_updated = ?\
      \ WHERE id = ? AND balance - free_sats >= ?"
      [ toPersistValue amountSats, toPersistValue now
      , toPersistValue key, toPersistValue amountSats
      ]
    (Debit, FreeSats) -> rawExecuteCount
      "UPDATE person SET balance = balance - ?, free_sats = free_sats - ?\
      \, last_updated = ? WHERE id = ? AND free_sats >= ?"
      [ toPersistValue amountSats, toPersistValue amountSats
      , toPersistValue now, toPersistValue key, toPersistValue amountSats
      ]
    (Credit, RealSats) -> updateWhereCount
      [ PersonId ==. key ]
      [ PersonBalance +=. amountSats
      , PersonLastUpdated =. now
      ]
    (Credit, FreeSats) -> updateWhereCount
      [ PersonId ==. key ]
      [ PersonBalance +=. amountSats
      , PersonFreeSats +=. amountSats
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
    , ledgerEntryFundsKind = funds
    , ledgerEntryReason = reason
    , ledgerEntryBalanceAfter = balanceAfter
    , ledgerEntryReference = reference
    , ledgerEntryCreatedAt = now
    }
  return balanceAfter

-- | whether a reason can describe a movement in the given direction.
--
-- The two were taken as unrelated, so a caller could record a credit as a
-- Withdrawal or a debit as Winnings: the balance moved correctly, while the
-- entry explaining it said the opposite, and the wallet history renders the
-- kind of a row from its reason and the sign from its direction -- a credit
-- reasoned Withdrawal shows as money sent, in the colour of money sent,
-- carrying a positive number.
--
-- Written out per pair rather than with a catch-all, so a reason added
-- later does not silently inherit an answer
reasonDescribes :: LedgerDirection -> LedgerReason -> Bool
reasonDescribes Credit OpeningBalance = True
reasonDescribes Credit Deposit        = True
reasonDescribes Credit Refund         = True
reasonDescribes Credit Winnings       = True
reasonDescribes Credit Withdrawal     = False
reasonDescribes Credit Stake          = False
reasonDescribes Debit  Withdrawal     = True
reasonDescribes Debit  Stake          = True
reasonDescribes Debit  OpeningBalance = False
reasonDescribes Debit  Deposit        = False
reasonDescribes Debit  Refund         = False
reasonDescribes Debit  Winnings       = False

-- | reference of an entry, which relates to a record of another service,
-- eg @ledgerReference "offer" "42"@ is @"offer:42"@
ledgerReference :: Text -> Text -> Text
ledgerReference kind recordId = kind <> ":" <> recordId

-- | 'ledgerReference' of a record, which is identified by a number
ledgerReferenceOfKey :: Text -> Int64 -> Text
ledgerReferenceOfKey kind key =
  ledgerReference kind (Text.pack (show key))
