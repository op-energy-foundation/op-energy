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

import           Control.Monad.Logger (NoLoggingT)
import           Control.Monad.Trans.Reader (ReaderT)
import           Control.Monad.Trans.Resource (ResourceT)

import           Database.Persist (toPersistValue)
import           Database.Persist.Sql (SqlBackend, rawExecute)

import           Data.OpEnergy.Account.API.V2.LedgerReason (LedgerReason(..))

import           OpEnergy.Account.Server.V1.Config (Config)
import           OpEnergy.Account.Server.V1.LedgerEntry (LedgerDirection(..))

-- | records the current balance of every account without ledger entries as
-- its opening balance.
--
-- One statement rather than a pass over the accounts: this runs while the
-- pool is being built, so the service answers nothing until it returns,
-- and a row per account -- each with a count of its entries before it --
-- would hold the whole account table in memory and spend two round trips
-- on every account, including the accounts, which already have an entry
-- and are the normal case once this has run.
--
-- The direction and the reason are passed as parameters rather than
-- written into the statement, so the strings, which reach the DB, are the
-- ones their PersistField instances produce.
migration
  :: Config
  -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) ()
migration _config =
  rawExecute
    ( "INSERT INTO ledger_entry"
   <> " (person_id, direction, amount_sats, reason, balance_after"
   <> " , reference, created_at)"
   <> " SELECT p.id, ?, p.balance, ?, p.balance, NULL, p.creation_time"
   <> " FROM person p"
   <> " WHERE NOT EXISTS"
   <> " (SELECT 1 FROM ledger_entry l WHERE l.person_id = p.id)"
    )
    [ toPersistValue Credit
    , toPersistValue OpeningBalance
    ]
