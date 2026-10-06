{-- | This migration gives accounts, which were registered before the ledger
 - existed, the opening-balance entry a new account now gets: without it the
 - ledger of such an account would not sum to its balance and its wallet
 - history would start empty.
 -
 - Idempotent: an account, which already has an entry, is left alone.
 -}
{-# LANGUAGE OverloadedStrings          #-}
module OpEnergy.Account.Server.V1.DB.Migrations.BackfillLedgerOpeningBalance.Migration
  ( migration
  ) where

import           Control.Monad (forM_, unless)
import           Control.Monad.Logger (NoLoggingT)
import           Control.Monad.Trans.Reader (ReaderT)
import           Control.Monad.Trans.Resource (ResourceT)

import           Database.Persist
import           Database.Persist.Sql (SqlBackend)

import           Data.OpEnergy.Account.API.V2.LedgerReason (LedgerReason(..))

import           OpEnergy.Account.Server.V1.Config (Config)
import           OpEnergy.Account.Server.V1.LedgerEntry
import           OpEnergy.Account.Server.V1.Person

-- | records the current balance of every account without ledger entries as
-- its opening balance
migration
  :: Config
  -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) ()
migration _config = do
  persons <- selectList [] [ Asc PersonId ]
  forM_ persons $ \(Entity key person) -> do
    existing <- count [ LedgerEntryPersonId ==. key ]
    unless (existing > 0) $ insert_ $ LedgerEntry
      { ledgerEntryPersonId = key
      , ledgerEntryDirection = Credit
      , ledgerEntryAmountSats = personBalance person
      , ledgerEntryReason = OpeningBalance
      , ledgerEntryBalanceAfter = personBalance person
      , ledgerEntryReference = Nothing
      , ledgerEntryCreatedAt = personCreationTime person
      }
