{-- | Wallet backend, which needs no lightning node: invoices and payments
 - are kept in this service's own database.
 -
 - It exists so the wallet can be built, deployed and tested before the node
 - and the LNBits instance are available. It behaves like the real one from
 - the outside: an invoice has to be created before it can be paid, a
 - payment is recorded once, and an invoice this service issued for another
 - account is settled as a transfer inside the service rather than over
 - lightning.
 -}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module OpEnergy.Account.Server.V1.Wallet.Mock
  ( mockWalletBackend
  ) where

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BS8
import           Data.Int (Int64)
import           Data.Pool (Pool)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import           Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import           Data.Word (Word64)
import qualified System.Random as Random

import           Database.Persist
import           Database.Persist.Sql (SqlBackend, fromSqlKey, updateWhereCount)
import           Database.Persist.Postgresql (runSqlPersistMPool)

import           Data.OpEnergy.Account.API.V1.Sats (Sats(..))
import           Data.OpEnergy.Account.API.V2.Bolt11Invoice
                 ( Bolt11Invoice(..), everifyBolt11Invoice
                 )
import           Data.OpEnergy.Account.API.V2.PaymentHash
                 ( PaymentHash(..), everifyPaymentHash
                 )
import           Data.OpEnergy.Account.API.V2.WalletMode (WalletMode(..))

import           OpEnergy.Account.Server.V1.Person (PersonId)
import           OpEnergy.Account.Server.V1.Wallet.Class
import           OpEnergy.Account.Server.V1.WalletPayment
import           OpEnergy.Error
                 ( CallstackError, walletAmountRequired
                 , walletInvoiceAlreadyPaid, walletInvoiceNotFound
                 , walletSelfPaymentNotAllowed
                 )

-- | wallet backend, which keeps invoices and payments in the given database
mockWalletBackend :: Pool SqlBackend -> WalletBackend
mockWalletBackend pool = WalletBackend
  { walletKind = WalletModeMock
  , walletCreateInvoice = createInvoice pool
  , walletResolveInvoice = resolveInvoice pool
  , walletPayInvoice = payInvoice pool
  , walletSettleInvoice = Just (settleInvoice pool)
  }

-- | records an invoice, which anyone may settle through the mock's settle
-- procedure. The invoice is not a payable lightning invoice: no node issued
-- it, so it is marked as this service's own
createInvoice
  :: Pool SqlBackend
  -> CreateInvoiceParams
  -> IO (Either CallstackError CreatedInvoice)
createInvoice pool params = do
  entropy <- Random.randomIO :: IO Word64
  let personKey = createInvoicePersonId params
      amount = createInvoiceAmountSats params
      now = createInvoiceNow params
      expiresAt =
        now + fromIntegral (createInvoiceExpirySecs params)
      paymentHashV = mockPaymentHash personKey amount now entropy
      invoiceV = mockInvoice paymentHashV
  _ <- flip runSqlPersistMPool pool $ insert $ WalletPayment
    { walletPaymentPersonId = personKey
    , walletPaymentDirection = Incoming
    , walletPaymentStatus = Pending
    , walletPaymentAmountSats = amount
    , walletPaymentFeeSats = Sats 0
    , walletPaymentPaymentHash = paymentHashV
    , walletPaymentInvoice = invoiceV
    , walletPaymentNote = createInvoiceNote params
    , walletPaymentExpiresAt = Just expiresAt
    , walletPaymentCreatedAt = now
    , walletPaymentUpdatedAt = now
    }
  return $! Right $! CreatedInvoice
    { createdInvoicePaymentHash = paymentHashV
    , createdInvoiceBolt11 = invoiceV
    , createdInvoiceAmountSats = amount
    , createdInvoiceExpiresAt = expiresAt
    }

-- | an invoice this service issued is known by its own row; anything else
-- can not be read without a node
resolveInvoice
  :: Pool SqlBackend
  -> Bolt11Invoice
  -> IO (Either CallstackError (Maybe ResolvedInvoice))
resolveInvoice pool invoiceV = do
  mours <- flip runSqlPersistMPool pool $ selectFirst
    [ WalletPaymentInvoice ==. invoiceV
    , WalletPaymentDirection ==. Incoming
    ]
    [ Desc WalletPaymentId ]
  return $! Right $! case mours of
    Nothing -> Nothing
    Just (Entity _ payment) -> Just $! ResolvedInvoice
      { resolvedInvoiceAmountSats = walletPaymentAmountSats payment
      , resolvedInvoicePersonId = Just (walletPaymentPersonId payment)
      , resolvedInvoicePayable = walletPaymentStatus payment == Pending
      }

-- | pays an invoice: one this service issued for another account becomes a
-- transfer inside the service, anything else is recorded as having left.
-- The caller moves the balances; this only records the payment
payInvoice
  :: Pool SqlBackend
  -> PayInvoiceParams
  -> IO (Either CallstackError PaidInvoice)
payInvoice pool params = do
  entropy <- Random.randomIO :: IO Word64
  let payer = payInvoicePersonId params
      invoiceV = payInvoiceBolt11 params
      now = payInvoiceNow params
  mours <- flip runSqlPersistMPool pool $ selectFirst
    [ WalletPaymentInvoice ==. invoiceV
    , WalletPaymentDirection ==. Incoming
    ]
    [ Desc WalletPaymentId ]
  case mours of
    Just (Entity key payment)
      | walletPaymentPersonId payment == payer ->
          return $! Left walletSelfPaymentNotAllowed
      | walletPaymentStatus payment /= Pending ->
          return $! Left walletInvoiceNotFound
      | otherwise -> do
          settled <- flip runSqlPersistMPool pool $ updateWhereCount
            [ WalletPaymentId ==. key, WalletPaymentStatus ==. Pending ]
            [ WalletPaymentStatus =. Settled, WalletPaymentUpdatedAt =. now ]
          if settled /= (1 :: Int64)
            then return $! Left walletInvoiceNotFound
            else return $! Right $! PaidInvoice
              { paidInvoicePaymentHash = walletPaymentPaymentHash payment
              , paidInvoiceAmountSats = walletPaymentAmountSats payment
              , paidInvoiceFeeSats = Sats 0
              , paidInvoiceRecipientPersonId = Just (walletPaymentPersonId payment)
              }
    Nothing -> do
      malreadyPaid <- flip runSqlPersistMPool pool $ selectFirst
        [ WalletPaymentInvoice ==. invoiceV
        , WalletPaymentDirection ==. Outgoing
        ]
        []
      case (malreadyPaid, payInvoiceAmountSats params) of
        -- a node would refuse an invoice it has already paid; this wallet
        -- has no node, so its own record of the payment is what refuses it
        (Just _, _) -> return $! Left walletInvoiceAlreadyPaid
        -- an invoice, which this service did not issue, can not be decoded
        -- without a node, so the amount has to be given and is checked
        -- against the account's balance by the caller
        (Nothing, Nothing) -> return $! Left walletAmountRequired
        (Nothing, Just amount) -> do
          let paymentHashV = mockPaymentHash payer amount now entropy
          _ <- flip runSqlPersistMPool pool $ insert $ WalletPayment
            { walletPaymentPersonId = payer
            , walletPaymentDirection = Outgoing
            , walletPaymentStatus = Settled
            , walletPaymentAmountSats = amount
            , walletPaymentFeeSats = Sats 0
            , walletPaymentPaymentHash = paymentHashV
            , walletPaymentInvoice = invoiceV
            , walletPaymentNote = Nothing
            , walletPaymentExpiresAt = Nothing
            , walletPaymentCreatedAt = now
            , walletPaymentUpdatedAt = now
            }
          return $! Right $! PaidInvoice
            { paidInvoicePaymentHash = paymentHashV
            , paidInvoiceAmountSats = amount
            , paidInvoiceFeeSats = Sats 0
            , paidInvoiceRecipientPersonId = Nothing
            }

-- | marks an invoice of the given account paid, as a payer would. Reports
-- whether this call was the one, which settled it, so its account is
-- credited exactly once.
--
-- The invoice is looked up by its payment hash together with the account,
-- which asks, so an invoice of another account matches nothing: a payment
-- hash of somebody else can not settle their invoice, nor tell the caller
-- that it exists
settleInvoice
  :: Pool SqlBackend
  -> PersonId
  -> PaymentHash
  -> IO (Either CallstackError SettledInvoice)
settleInvoice pool personKey paymentHashV = do
  mpayment <- flip runSqlPersistMPool pool
    $ selectFirst
      [ WalletPaymentPaymentHash ==. paymentHashV
      , WalletPaymentPersonId ==. personKey
      ]
      []
  case mpayment of
    Nothing -> return $! Left walletInvoiceNotFound
    Just (Entity key payment)
      | walletPaymentDirection payment /= Incoming ->
          return $! Left walletInvoiceNotFound
      | otherwise -> do
          now <- getPOSIXTime
          settled <- flip runSqlPersistMPool pool $ updateWhereCount
            [ WalletPaymentId ==. key
            , WalletPaymentPersonId ==. personKey
            , WalletPaymentStatus ==. Pending
            ]
            [ WalletPaymentStatus =. Settled, WalletPaymentUpdatedAt =. now ]
          return $! Right $! SettledInvoice
            { settledInvoicePersonId = walletPaymentPersonId payment
            , settledInvoicePaymentHash = paymentHashV
            , settledInvoiceAmountSats = walletPaymentAmountSats payment
            , settledInvoiceWasPending = settled == (1 :: Int64)
            }

-- | payment hash of a mock payment: the hash of what the payment is, so two
-- payments never share one
mockPaymentHash :: PersonId -> Sats -> POSIXTime -> Word64 -> PaymentHash
mockPaymentHash personKey (Sats amount) now entropy =
  either (error . Text.unpack) id $ everifyPaymentHash $ TE.decodeUtf8
    $ B16.encode $ SHA256.hash $ BS8.pack
    $ show (fromSqlKey personKey) <> ":" <> show amount
      <> ":" <> show (realToFrac now :: Double) <> ":" <> show entropy

-- | invoice of a mock payment. It is marked as belonging to this service, so
-- nobody mistakes it for an invoice a node would pay
mockInvoice :: PaymentHash -> Bolt11Invoice
mockInvoice (PaymentHash hashText) =
  either (error . Text.unpack) id $ everifyBolt11Invoice
    $ "lntbsmock" <> hashText
