{-- | This module defines the wallet backend: what this service needs from a
 - lightning wallet, as a record of procedures.
 -
 - One value of 'WalletBackend' is built at startup from config and kept in
 - 'OpEnergy.Account.Server.V1.Class.State', so no handler knows which
 - backend it talks to. The mock implementation lives in
 - "OpEnergy.Account.Server.V1.Wallet.Mock"; the LNBits one replaces it when
 - the node is available, with no change to the handlers or to the API.
 -
 - The procedures answer about lightning only. Balances are moved by the
 - handlers through 'OpEnergy.Account.Server.V1.LedgerEntry.adjustBalanceTx',
 - which keeps the ledger the single record of every balance change.
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
  , payInvoiceNow :: POSIXTime
  }
  deriving (Show, Generic)

-- | a payment, which has left this service
data PaidInvoice = PaidInvoice
  { paidInvoicePaymentHash :: PaymentHash
  , paidInvoiceAmountSats :: Sats
  , paidInvoiceFeeSats :: Sats
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
  }
  deriving (Show, Generic)

-- | everything this service needs from a lightning wallet
data WalletBackend = WalletBackend
  { walletKind :: WalletMode
  , walletCreateInvoice
      :: CreateInvoiceParams -> IO (Either CallstackError CreatedInvoice)
  , walletResolveInvoice
      :: Bolt11Invoice -> IO (Either CallstackError (Maybe ResolvedInvoice))
    -- ^ what the wallet can tell about an invoice before paying it.
    -- 'Nothing' when it can not read it, and the amount then has to come
    -- from the client
  , walletPayInvoice
      :: PayInvoiceParams -> IO (Either CallstackError PaidInvoice)
  , walletSettleInvoice
      :: Maybe
         (PersonId -> PaymentHash -> IO (Either CallstackError SettledInvoice))
    -- ^ 'Just' only for the mock backend, which has no real payer: marking
    -- an invoice paid is impossible against a real wallet, so the endpoint
    -- offering it can not be served at all once this is 'Nothing'.
    -- The 'PersonId' is the account, which asks: an invoice of any other
    -- account is not found, so no call can reach one
  }
