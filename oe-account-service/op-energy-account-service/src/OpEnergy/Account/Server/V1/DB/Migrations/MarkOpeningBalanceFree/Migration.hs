{-- | This migration marks the sats every existing account was given at
 - registration as free sats: they can be staked, but not withdrawn.
 -
 - Until the two pockets existed, a balance was one number, so an account
 - could pay its opening balance out over lightning and register again for
 - more. Accounts, which were created before this change, carry that balance
 - still, so the grant is marked here rather than only for accounts created
 - from now on.
 -
 - An account, which has since spent part of the grant, keeps only what is
 - left of it: the free part can never be more than the balance. An account,
 - which has deposited or won on top of it, keeps that as real sats.
 -
 - Idempotent: an account, whose free sats are already set, is left alone, so
 - free sats, which an account has won since, are not reset to the grant.
 -}
{-# LANGUAGE OverloadedStrings          #-}
module OpEnergy.Account.Server.V1.DB.Migrations.MarkOpeningBalanceFree.Migration
  ( migration
  ) where

import           Control.Monad.Logger (NoLoggingT)
import           Control.Monad.Trans.Reader (ReaderT)
import           Control.Monad.Trans.Resource (ResourceT)

import           Database.Persist.Sql (SqlBackend, rawExecute)

import           OpEnergy.Account.Server.V1.Config (Config)

-- | marks every account's opening balance, and what is left of it, as free
-- sats.
--
-- Written in SQL rather than with Persistent, as both statements set a
-- column from other columns of the same rows, which Persistent's updates
-- cannot express.
migration
  :: Config
  -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) ()
migration _config = do
  -- person's own migration runs after the custom ones (see DB.hs), so the
  -- column this fills does not exist yet. Added here with IF NOT EXISTS,
  -- which leaves persistent's later ALTER nothing to do, rather than
  -- reordering a migration list other migrations were written against
  rawExecute
    "ALTER TABLE person ADD COLUMN IF NOT EXISTS free_sats INT8 NOT NULL\
    \ DEFAULT 0"
    []
  -- the entries, which explain the grant, say which pocket they credited
  rawExecute
    "UPDATE ledger_entry SET funds_kind = 'free'\
    \ WHERE reason = 'opening_balance' AND direction = 'credit'"
    []
  -- and the account's free sats are what is left of the grant: the whole of
  -- it, or the balance when part of it has already been staked away
  rawExecute
    "UPDATE person SET free_sats = LEAST(balance, COALESCE((\
    \ SELECT SUM(amount_sats) FROM ledger_entry\
    \  WHERE person_id = person.id\
    \    AND reason = 'opening_balance'\
    \    AND direction = 'credit'), 0))\
    \ WHERE free_sats = 0"
    []
