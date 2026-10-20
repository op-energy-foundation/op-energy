{-- | Wallet backend, which talks to an LNBits instance in front of a
 - lightning node, so the sats it reports are real ones.
 -
 - It receives only: an invoice is issued by LNBits and paid over
 - lightning, and this service learns that it was paid by asking, which the
 - scheduler does through 'walletReadPayment'. Paying an invoice out of the
 - wallet is refused, as a real payment is not instant and the handler,
 - which takes the balance first, has no state for one, which is still in
 - flight. 'walletCanSend' reports that, so a client is told before it
 - tries rather than by a refusal.
 -
 - LNBits is asked for amounts in satoshi and answers in millisatoshi;
 - every amount, which crosses this module, is converted here so nothing
 - outside it sees a millisatoshi.
 -
 - The API keys are secrets, which are sent as a header on every call. No
 - error or log line in this module renders a request or a 'ClientError',
 - as both carry the key.
 -}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module OpEnergy.Account.Server.V1.Wallet.LNBits
  ( lnbitsWalletBackend
  ) where

import           Data.Pool (Pool)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time.Clock.POSIX (POSIXTime)

import           Database.Persist (insertUnique)
import           Database.Persist.Sql (SqlBackend)
import           Database.Persist.Postgresql (runSqlPersistMPool)
import           Servant.Client
                 ( BaseUrl, ClientError(..), ClientM, responseStatusCode
                 )
import           Network.HTTP.Types.Status (statusCode)

import           Data.OpEnergy.Account.API.V1.Sats (Sats(..))
import           Data.OpEnergy.Account.API.V2.Bolt11Invoice
                 ( Bolt11Invoice, everifyBolt11Invoice
                 )
import           Data.OpEnergy.Account.API.V2.PaymentHash
                 ( PaymentHash(..), everifyPaymentHash
                 )
import           Data.OpEnergy.Account.API.V2.WalletMode (WalletMode(..))
import           Data.OpEnergy.Account.Client (withClientEither)

import           OpEnergy.Account.Server.V1.Wallet.Class
import qualified OpEnergy.Account.Server.V1.Wallet.LNBits.Client
                 as LNBits
                 ( CreateInvoiceRequest(..), PaymentResponse(..)
                 , PaymentStateResponse(..), createInvoice, readPaymentState
                 )
import           OpEnergy.Account.Server.V1.WalletPayment
import           OpEnergy.Error
                 ( CallstackError, eitherException, walletAPIError
                 , walletInvoiceAlreadyPaid, walletSendNotSupported
                 )

-- | wallet backend, which issues its invoices through the LNBits instance
-- at the given URL.
--
-- The invoice key reads and creates; the admin key spends. Only the
-- invoice key is used here, as this backend does not send: a path, which
-- only receives, never holds the key, which could empty the wallet
lnbitsWalletBackend
  :: BaseUrl
  -> Text
  -> Pool SqlBackend
  -> WalletBackend
lnbitsWalletBackend url invoiceKey pool = WalletBackend
  { walletKind = WalletModeLNBits
  , walletCanSend = False
  , walletCanReceive = True
  , walletCreateInvoice = createInvoice url invoiceKey pool
  -- refused before the handler takes any balance, rather than after it has
  -- been taken and given back
  , walletResolveInvoice = \_asker _invoice ->
      return $! Left walletSendNotSupported
  , walletPayInvoice = \_params -> return $! Left walletSendNotSupported
  , walletReadPayment = Just (readPayment url invoiceKey)
  -- an invoice of a real wallet is paid over lightning or not at all:
  -- there is no payer here, which could be asked to pretend
  , walletSettleInvoice = Nothing
  }

-- | asks LNBits for an invoice and records it, so the scheduler knows
-- there is a payment to ask about and the history has it.
--
-- The invoice is created before it is recorded, as its hash is LNBits'
-- answer and not something this service can choose. An invoice, which was
-- created and could not then be recorded, is reported as an error and
-- logged with its hash by the caller: the sats of a payer, who pays it
-- anyway, are in the LNBits wallet and are credited by hand. Nothing is
-- lost, and the window is one failed DB write wide
createInvoice
  :: BaseUrl
  -> Text
  -> Pool SqlBackend
  -> CreateInvoiceParams
  -> IO (Either CallstackError CreatedInvoice)
createInvoice url invoiceKey pool params = do
  let Sats amount = createInvoiceAmountSats params
      now = createInvoiceNow params
      expiresAt = now + fromIntegral (createInvoiceExpirySecs params)
  eanswer <- callLNBits url $ LNBits.createInvoice invoiceKey
    $ LNBits.CreateInvoiceRequest
      { LNBits.createInvoiceRequestAmountSats = amount
      , LNBits.createInvoiceRequestMemo = createInvoiceNote params
      , LNBits.createInvoiceRequestExpirySecs = createInvoiceExpirySecs params
      }
  case eanswer of
    Left err -> return $! Left err
    Right answer -> case readInvoiceAnswer answer of
      Left err -> return $! Left err
      Right (paymentHashV, invoiceV) ->
        recordInvoice pool params paymentHashV invoiceV expiresAt

-- | the hash and the invoice of an answer, verified as this service's own
-- types rather than trusted as the text they arrived as
readInvoiceAnswer
  :: LNBits.PaymentResponse
  -> Either CallstackError (PaymentHash, Bolt11Invoice)
readInvoiceAnswer answer = do
  paymentHashV <- orAnswerError
    $ everifyPaymentHash (LNBits.paymentResponsePaymentHash answer)
  invoiceV <- orAnswerError
    $ everifyBolt11Invoice (LNBits.paymentResponseBolt11 answer)
  return (paymentHashV, invoiceV)
  where
    -- the reason is the validator's own, which describes the shape it
    -- refused and does not carry the request
    orAnswerError = either
      (\reason-> Left $! walletAPIError
        ("the wallet answered with an invoice this service can not read: "
         <> reason))
      Right

-- | records a created invoice as an incoming payment waiting to be paid
recordInvoice
  :: Pool SqlBackend
  -> CreateInvoiceParams
  -> PaymentHash
  -> Bolt11Invoice
  -> POSIXTime
  -> IO (Either CallstackError CreatedInvoice)
recordInvoice pool params paymentHashV invoiceV expiresAt = do
  let now = createInvoiceNow params
      amount = createInvoiceAmountSats params
  -- insertUnique rather than insert: a hash, which is already recorded,
  -- would otherwise raise a constraint violation as an exception and
  -- answer the request with a bare 500
  einserted <- eitherException $ flip runSqlPersistMPool pool
    $ insertUnique $ WalletPayment
      { walletPaymentPersonId = createInvoicePersonId params
      , walletPaymentDirection = Incoming
      , walletPaymentStatus = Pending
      , walletPaymentAmountSats = amount
      , walletPaymentFeeSats = Sats 0
      , walletPaymentPaymentHash = paymentHashV
      , walletPaymentInvoice = invoiceV
      , walletPaymentNote = createInvoiceNote params
      , walletPaymentExpiresAt = Just expiresAt
      , walletPaymentCreditedAt = Nothing
      , walletPaymentCreatedAt = now
      , walletPaymentUpdatedAt = now
      }
  return $! case einserted of
    -- the invoice exists at the wallet and is not recorded here. Its hash
    -- is named so a payment, which arrives for it, can be found
    Left _ -> Left $! walletAPIError
      ( "an invoice was created but could not be recorded, so a payment "
     <> "of it needs crediting by hand. Its payment hash is "
     <> unPaymentHash paymentHashV
      )
    Right Nothing -> Left walletInvoiceAlreadyPaid
    Right (Just _) -> Right $! CreatedInvoice
      { createdInvoicePaymentHash = paymentHashV
      , createdInvoiceBolt11 = invoiceV
      , createdInvoiceAmountSats = amount
      -- the expiry this service asked for, rather than the one in the
      -- answer: LNBits reports it as a local time without a zone, which
      -- can not be read as an instant, and it is the requested one
      , createdInvoiceExpiresAt = expiresAt
      }

-- | asks LNBits what has become of a payment
readPayment
  :: BaseUrl
  -> Text
  -> PaymentHash
  -> IO (Either CallstackError ObservedPayment)
readPayment url invoiceKey (PaymentHash hashText) = do
  eanswer <- callLNBitsMaybe url $ LNBits.readPaymentState invoiceKey hashText
  return $! fmap (maybe ObservedUnknown observe) eanswer
  where
    -- "paid" is the explicit answer and is believed on its own; a status,
    -- which is not one of the words below, is reported as pending, so an
    -- answer this service does not understand never credits an account
    observe answer
      | LNBits.paymentStateResponsePaid answer = ObservedSettled
      | otherwise = case LNBits.paymentStateResponseStatus answer of
          "success" -> ObservedSettled
          "failed" -> ObservedFailed
          _ -> ObservedPending

-- | performs one call against the LNBits instance, turning every way it
-- can fail into an error value. An answer of "not there" is an error here
-- as well: 'callLNBitsMaybe' is for the caller, which has something to
-- make of it
callLNBits :: BaseUrl -> ClientM r -> IO (Either CallstackError r)
callLNBits url call = do
  eresult <- callLNBitsMaybe url call
  return $! case eresult of
    Left err -> Left err
    Right Nothing -> Left $! walletAPIError notFoundReason
    Right (Just result) -> Right result

-- | 'callLNBits', which reports an answer of "not there" as 'Nothing'
-- rather than as an error, for a caller, which asks about something, which
-- may not be the wallet's.
--
-- The reason of a failure is built from the status code alone. A
-- 'ClientError' renders the request, which carries the API key, so it is
-- never shown
callLNBitsMaybe
  :: BaseUrl
  -> ClientM r
  -> IO (Either CallstackError (Maybe r))
callLNBitsMaybe url call = do
  -- guarded as well as mapped: a manager, which can not be built, and a
  -- host, which can not be resolved, leave as exceptions rather than as a
  -- Left, and the caller has to answer the request either way
  eresult <- eitherException $ withClientEither url call
  return $! case eresult of
    Left _ -> Left $! walletAPIError "the wallet could not be reached"
    Right (Left clientError)
      | isNotFound clientError -> Right Nothing
      | otherwise -> Left $! walletAPIError (describeClientError clientError)
    Right (Right result) -> Right (Just result)

-- | whether the wallet answered that what was asked about is not there
isNotFound :: ClientError -> Bool
isNotFound (FailureResponse _request response) =
  statusCode (responseStatusCode response) == notFoundStatus
isNotFound _ = False

notFoundStatus :: Int
notFoundStatus = 404

notFoundReason :: Text
notFoundReason = "the wallet answered with status 404"

-- | what may be said about a failed call without naming the request
describeClientError :: ClientError -> Text
describeClientError (FailureResponse _request response) =
  "the wallet answered with status "
    <> Text.pack (show (statusCode (responseStatusCode response)))
describeClientError (DecodeFailure _reason _response) =
  "the wallet answered with something this service can not read"
describeClientError (UnsupportedContentType _mediaType _response) =
  "the wallet answered in a format this service can not read"
describeClientError (InvalidContentTypeHeader _response) =
  "the wallet answered without a format this service can read"
describeClientError (ConnectionError _exception) =
  "the wallet could not be reached"
