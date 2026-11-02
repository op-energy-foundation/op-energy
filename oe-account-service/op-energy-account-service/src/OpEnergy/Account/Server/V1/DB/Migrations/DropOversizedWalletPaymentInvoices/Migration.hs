{-- | This migration removes wallet_payment rows whose invoice holds more
 - bytes than an invoice is allowed to.
 -
 - Such a row can only have been written by a build whose check on an
 - invoice counted characters and accepted every alphanumeric Unicode has: a
 - conformant one stores at most 'maxInvoiceLength' ASCII characters, which
 - is that many bytes. A row, which holds more, is a value no build of this
 - service would accept today, so it is dropped rather than left to be read
 - back as an invoice.
 -
 - It does not unblock the unique index over the column: that index is
 - created by @runMigration migrateWalletPayment@, which runs before the
 - custom migrations, so a row too large for a btree entry would stop the
 - service before this statement is reached. This is hygiene over rows,
 - which are small enough to store and still larger than an invoice may be.
 -
 - The balance, which such a payment moved, is recorded in ledger_entry and
 - is left alone: what goes is the mock wallet's own record of a payment to
 - an invoice no wallet could have paid.
 -
 - Idempotent: a conformant DB has no such row and the statement deletes
 - nothing.
 -}
{-# LANGUAGE OverloadedStrings          #-}
module OpEnergy.Account.Server.V1.DB.Migrations.DropOversizedWalletPaymentInvoices.Migration
  ( migration
  ) where

import           Control.Monad.Logger (NoLoggingT)
import           Control.Monad.Trans.Reader (ReaderT)
import           Control.Monad.Trans.Resource (ResourceT)

import           Database.Persist (PersistValue(..))
import           Database.Persist.Sql (SqlBackend, rawExecute)

import           Data.OpEnergy.Account.API.V2.Bolt11Invoice (maxInvoiceLength)

import           OpEnergy.Account.Server.V1.Config (Config)

-- | deletes every payment whose stored invoice is longer, in bytes, than an
-- invoice may be
migration
  :: Config
  -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) ()
migration _config = rawExecute
  "DELETE FROM wallet_payment WHERE octet_length(invoice) > ?"
  [ PersistInt64 (fromIntegral maxInvoiceLength) ]
