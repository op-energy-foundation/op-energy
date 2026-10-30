{-- | This module defines the wallet backend: what this service needs from a
 - lightning wallet, as a record of procedures.
 -
 - One value of 'WalletBackend' is built at startup from config and kept in
 - 'OpEnergy.Account.Server.V1.Class.State', so no handler knows which
 - backend it talks to, beyond 'GetWalletInfo', which reports which one it
 - is. The mock implementation lives in
 - "OpEnergy.Account.Server.V1.Wallet.Mock" and the one, which talks to a
 - real wallet, in "OpEnergy.Account.Server.V1.Wallet.LNBits"; which of
 - them is built changes neither the handlers nor the API.
 -
 - A backend moves the paying account's balance itself, in the transaction,
 - which records the payment, so the two can not be written apart. It does
 - so through 'OpEnergy.Account.Server.V1.LedgerEntry.adjustBalanceTx',
 - which stays the single record of every balance change.
 -}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}
module OpEnergy.Account.Server.V1.Wallet.Class
  ( WalletBackend(..)
  , CreateInvoiceParams(..)
  , CreatedInvoice(..)
  , PayInvoiceParams(..)
  , PaidInvoice(..)
  , SettledInvoice(..)
  , ResolvedInvoice(..)
  , ObservedPayment(..)
  , PaymentProgress(..)
  ) where

import           Data.Text (Text)
import           Data.Time.Clock.POSIX (POSIXTime)
import           Data.Word (Word64)
import           GHC.Generics

import           Data.OpEnergy.Account.API.V1.Sats (Sats(..))
import           Data.OpEnergy.Account.API.V2.Bolt11Invoice (Bolt11Invoice(..))
import           Data.OpEnergy.Account.API.V2.PaymentHash (PaymentHash(..))
import           Data.OpEnergy.Account.API.V2.WalletMode (WalletMode(..))

import           OpEnergy.Account.Server.V1.Person (PersonId)
import           OpEnergy.Error (CallstackError)

-- | what an account asks for when it wants to be paid
data CreateInvoiceParams = CreateInvoiceParams
  { createInvoicePersonId :: PersonId
  , createInvoiceAmountSats :: Sats
  , createInvoiceNote :: Maybe Text
  , createInvoiceExpirySecs :: Word64
  , createInvoiceNow :: POSIXTime
  }
  deriving (Show, Generic)

-- | the invoice to show, as text and as a QR code
data CreatedInvoice = CreatedInvoice
  { createdInvoicePaymentHash :: PaymentHash
  , createdInvoiceBolt11 :: Bolt11Invoice
  , createdInvoiceAmountSats :: Sats
  , createdInvoiceExpiresAt :: POSIXTime
  }
  deriving (Show, Generic)

-- | what an account asks for when it wants to pay someone
data PayInvoiceParams = PayInvoiceParams
  { payInvoicePersonId :: PersonId
  , payInvoiceBolt11 :: Bolt11Invoice
  , payInvoiceAmountSats :: Maybe Sats
    -- ^ amount the client believes the invoice is for. Used only when the
    -- backend can not tell the amount itself, and never trusted beyond that
  , payInvoiceFeeReserveSats :: Sats
    -- ^ what is taken from the balance on top of the amount, to cover a
    -- fee, which can not be known yet. The backend takes it with the
    -- amount and records it against the payment, so whoever settles the
    -- payment gives back what the fee did not use
  , payInvoiceNow :: POSIXTime
  }
  deriving (Show, Generic)

-- | how far a payment got before the wallet answered
data PaymentProgress
  = PaymentSettled
    -- ^ the sats have left and the fee is the one, which was charged
  | PaymentInFlight
    -- ^ the wallet has taken the payment and is still trying. It may yet
    -- succeed, so the balance stays taken and the fee reported is the
    -- reserve rather than a charge. The scheduler asks again later
  deriving (Show, Eq, Ord, Generic, Enum, Bounded)

-- | a payment, which has left this service
data PaidInvoice = PaidInvoice
  { paidInvoicePaymentHash :: PaymentHash
  , paidInvoiceBalanceAfter :: Sats
    -- ^ what the paying account's balance is once the backend has moved
    -- it. The backend takes the amount and the reserve in the transaction,
    -- which records the payment, and gives back whatever the payment did
    -- not use in the transaction, which finishes it, so a balance and the
    -- payment it belongs to are never recorded apart
  , paidInvoiceAmountSats :: Sats
  , paidInvoiceFeeSats :: Sats
  , paidInvoiceProgress :: PaymentProgress
    -- ^ whether the wallet is done with this payment. A payment, which is
    -- still in flight, must not be refunded: it may still be paid, and
    -- refunding it pays it twice
  , paidInvoiceRecipientPersonId :: Maybe PersonId
    -- ^ set when the invoice was issued by this service for another account:
    -- the sats stay inside and that account is credited
  }
  deriving (Show, Generic)

-- | an invoice, which has just been paid
data SettledInvoice = SettledInvoice
  { settledInvoicePersonId :: PersonId
  , settledInvoicePaymentHash :: PaymentHash
  , settledInvoiceAmountSats :: Sats
  , settledInvoiceWasPending :: Bool
    -- ^ False when the invoice had already been settled, so the caller
    -- credits the balance once and only once
  }
  deriving (Show, Generic)

-- | an invoice, which this wallet can read: its amount is the wallet's
-- answer, never a number a client sent
data ResolvedInvoice = ResolvedInvoice
  { resolvedInvoiceAmountSats :: Sats
  , resolvedInvoicePersonId :: Maybe PersonId
    -- ^ set when this service issued the invoice for one of its accounts
  , resolvedInvoicePayable :: Bool
    -- ^ False once the invoice has been paid. An invoice is resolved either
    -- way, so the caller can refuse a paid one before it moves any balance
  , resolvedInvoicePaidByAsker :: Bool
    -- ^ True when the account, which asked, has paid this invoice already.
    -- Reported here so the caller refuses it before it takes the balance,
    -- rather than taking it and giving it straight back
  }
  deriving (Show, Generic)

-- | what a wallet says has become of a payment it was given the hash of.
-- Named apart from 'OpEnergy.Account.Server.V1.WalletPayment' statuses on
-- purpose: this is the wallet's answer, while those are what this service
-- has recorded, and the two are only brought together by the caller
data ObservedPayment
  = ObservedPending -- ^ nobody has paid it yet, or the payment is in flight
  | ObservedSettled Sats
    -- ^ the sats have moved, and the fee the wallet charged for it. The
    -- fee is zero for a payment, which arrived, and is what an outgoing
    -- payment cost
  | ObservedFailed -- ^ it will not be paid: expired, cancelled or rejected
  | ObservedUnknown
    -- ^ the wallet does not have this payment at all. A payment, which was
    -- recorded against another wallet -- by another backend, or by this
    -- one before its instance was changed -- reads this way, and so does
    -- one, which was never created. The caller decides what to make of it:
    -- the wallet is only reporting that it is not its own
  deriving (Show, Eq, Ord, Generic)

-- | everything this service needs from a lightning wallet
data WalletBackend = WalletBackend
  { walletKind :: WalletMode
  , walletCanSend :: Bool
    -- ^ whether this wallet can pay an invoice at all, so a client is told
    -- what it may offer rather than being left to find out from a refusal
  , walletCanReceive :: Bool
    -- ^ whether this wallet can issue an invoice to be paid
  , walletCreateInvoice
      :: CreateInvoiceParams -> IO (Either CallstackError CreatedInvoice)
  , walletResolveInvoice
      :: PersonId
      -> Bolt11Invoice
      -> IO (Either CallstackError (Maybe ResolvedInvoice))
    -- ^ what the wallet can tell about an invoice before paying it.
    -- 'Nothing' when it can not read it, and the amount then has to come
    -- from the client
  , walletPayInvoice
      :: PayInvoiceParams -> IO (Either CallstackError PaidInvoice)
    -- ^ pays an invoice and moves the paying account's balance for it.
    --
    -- The balance is the backend's to move, as only the backend knows the
    -- payment it belongs to: it takes the amount and the reserve in the
    -- transaction, which records the payment, and gives back what a
    -- payment, which did not go, or a fee, which was smaller than the
    -- reserve, left unused -- each in the transaction, which records
    -- that. So a 'Left' means the balance stands as it did, and the
    -- caller has nothing to give back
  , walletReadPayment
      :: Maybe (PaymentHash -> IO (Either CallstackError ObservedPayment))
    -- ^ 'Just' for a wallet, which settles payments elsewhere: this service
    -- is not told when an invoice is paid, so it has to ask, and the
    -- scheduler does. 'Nothing' for a backend, which settles payments
    -- itself and so already knows -- there is nothing to ask it.
    --
    -- The hash alone identifies the payment, as this service only ever
    -- asks about payments it recorded
  , walletSettleInvoice
      :: Maybe
         (PersonId -> PaymentHash -> IO (Either CallstackError SettledInvoice))
    -- ^ 'Just' only for the mock backend, which has no real payer: marking
    -- an invoice paid is impossible against a real wallet, so the endpoint
    -- offering it can not be served at all once this is 'Nothing'.
    -- The 'PersonId' is the account, which asks: an invoice of any other
    -- account is not found, so no call can reach one
  }
