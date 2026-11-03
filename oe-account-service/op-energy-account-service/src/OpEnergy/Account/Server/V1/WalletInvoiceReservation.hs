{-- | This module defines the WalletInvoiceReservation entity: one row per
 - invoice, which is being created right now.
 -
 - An account may have only so many invoices waiting at once. The invoice
 - itself is made at the wallet before there is anything to count: the
 - wallet is asked, it answers with the hash and the invoice string, and
 - only then is the payment written down. Counting the payments alone
 - therefore says nothing about the calls, which are still in flight, so a
 - burst of requests all read the same count and all go on to make one.
 -
 - The limit can only be held before the wallet is asked. Holding it
 - afterwards would mean refusing an invoice, which the wallet has already
 - made: it would stay payable with no row to credit against, and sats paid
 - to it would arrive with nobody to give them to. That is worse than the
 - burst it would prevent.
 -
 - So a reservation stands for one call, which is in flight. It is taken in
 - the same transaction, which counts, and the account's row is locked
 - first, so two requests of one account count one after the other instead
 - of both reading the same number. It is given back once the call is over,
 - whether the wallet answered or not.
 -
 - A reservation, which is never given back -- the process stopped between
 - taking it and the wallet answering -- stops counting once it is older
 - than 'invoiceReservationTtl', rather than counting for good. A slot held
 - by nothing would otherwise leave the account unable to ask to be paid,
 - and no answer from the wallet is waited on for longer than that anyway.
 - The scheduler deletes them once they have stopped counting.
 -}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
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
module OpEnergy.Account.Server.V1.WalletInvoiceReservation
  where

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Reader (ReaderT)
import           Data.Time.Clock.POSIX (POSIXTime)
import           Database.Persist
import           Data.Int (Int64)
import           Database.Persist.Sql (Single, SqlBackend, rawSql)
import           Database.Persist.TH
import           GHC.Generics (Generic)

import           OpEnergy.Account.Server.V1.Person (PersonId)
import           OpEnergy.Account.Server.V1.WalletPayment
                 ( EntityField(..), WalletPaymentDirection(..)
                 , WalletPaymentStatus(..)
                 )

share [mkPersist sqlSettings, mkMigrate "migrateWalletInvoiceReservation"]
  [persistLowerCase|
WalletInvoiceReservation
  personId PersonId -- local relation, as the row is always written where the key is known
  createdAt POSIXTime -- when the call, which this stands for, started
  deriving Eq Show Generic
|]

-- | how long a reservation counts against the limit.
--
-- It stands for a call to the wallet, which is in flight, so it only has
-- to outlast that call. Past it the reservation is taken to belong to a
-- call, which will never finish, and stops counting
invoiceReservationTtl :: POSIXTime
invoiceReservationTtl = 60

-- | locks the account's row for the rest of the transaction, and answers
-- how many rows that was (one, for an account, which exists).
--
-- Taken before anything is counted, so the requests of one account are
-- accounted for one after another: without it two of them read the same
-- count and each take the last slot. Read with rawSql rather than
-- rawExecute, as the statement answers a row and execute refuses a
-- statement, which does
lockPersonTx
  :: MonadIO m
  => PersonId
  -> ReaderT SqlBackend m Int
lockPersonTx personKey = do
  rows <- rawSql "SELECT id FROM person WHERE id = ? FOR UPDATE"
    [ toPersistValue personKey ]
  return $! length (rows :: [Single Int64])

-- | whether an account has room for another invoice: the invoices, which
-- are waiting for a payment, and the slots, which are held for calls in
-- flight, together come to fewer than the limit.
--
-- The limit is how many an account may have at once, so an account, which
-- already has that many, has no room: at the limit this is False.
--
-- Example:
--
-- > hasInvoiceSlot 10 9 0 == True
hasInvoiceSlot
  :: Int
     -- ^ how many invoices an account may have waiting at once
  -> Int
     -- ^ invoices, which are waiting for a payment
  -> Int
     -- ^ slots, which are held for calls in flight
  -> Bool
hasInvoiceSlot limit waiting held = waiting + held < limit

-- | takes a slot for an invoice, which is about to be made, and answers
-- its reservation -- or 'Nothing' when the account has no slot left.
--
-- The account's row is locked before anything is counted, so the requests
-- of one account are accounted for one after another: without it two of
-- them read the same count and each take the last slot. The lock is held
-- for this transaction only, which does not call the wallet, so a slow
-- wallet never holds it.
--
-- Both the invoices, which are waiting, and the slots, which are held for
-- calls in flight, are counted. An expired invoice is not: it is no longer
-- asked about, and the account would otherwise be left unable to ask to be
-- paid.
--
-- Example:
--
-- > claimInvoiceSlotTx 10 invoiceReservationTtl personKey now
claimInvoiceSlotTx
  :: MonadIO m
  => Int
     -- ^ how many invoices an account may have waiting at once
  -> POSIXTime
     -- ^ how long a slot, which is held for a call, counts for
  -> PersonId
  -> POSIXTime
  -> ReaderT SqlBackend m (Maybe (Key WalletInvoiceReservation))
claimInvoiceSlotTx limit ttl personKey now = do
  _ <- lockPersonTx personKey
  waiting <- count
    [ WalletPaymentPersonId ==. personKey
    , WalletPaymentDirection ==. Incoming
    , WalletPaymentStatus ==. Pending
    , WalletPaymentExpiresAt >. Just now
    ]
  held <- count
    [ WalletInvoiceReservationPersonId ==. personKey
    , WalletInvoiceReservationCreatedAt >. now - ttl
    ]
  if not (hasInvoiceSlot limit waiting held)
    then return Nothing
    else fmap Just $ insert $ WalletInvoiceReservation
      { walletInvoiceReservationPersonId = personKey
      , walletInvoiceReservationCreatedAt = now
      }

-- | gives a slot back, once the call it was held for is over. Called
-- whether the wallet answered or not: a slot, which is given back, is one
-- the account can use again at once, while one, which is not, has to wait
-- out 'invoiceReservationTtl' first
releaseInvoiceSlotTx
  :: MonadIO m
  => Key WalletInvoiceReservation
  -> ReaderT SqlBackend m ()
releaseInvoiceSlotTx = delete

-- | deletes the slots, which have stopped counting. Run from the
-- scheduler, so a slot, which was never given back, does not sit in the
-- table for good. It has already stopped counting by then, so this only
-- keeps the table small
dropStaleInvoiceSlotsTx
  :: MonadIO m
  => POSIXTime
     -- ^ how long a slot counts for
  -> POSIXTime
     -- ^ now
  -> ReaderT SqlBackend m ()
dropStaleInvoiceSlotsTx ttl now = deleteWhere
  [ WalletInvoiceReservationCreatedAt <=. now - ttl ]
