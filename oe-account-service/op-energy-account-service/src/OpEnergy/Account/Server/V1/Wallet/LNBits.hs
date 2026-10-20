{-- | Wallet backend, which talks to an LNBits instance in front of a
 - lightning node, so the sats it reports are real ones.
 -
 - An invoice is issued by LNBits and paid over lightning, and this service
 - learns that it was paid by asking, which the scheduler does through
 - 'walletReadPayment'.
 -
 - A payment out of the wallet is not instant: LNBits may still be trying
 - when it answers. Such a payment is reported as 'PaymentInFlight' and is
 - never treated as failed, as refunding one, which is still travelling,
 - pays it twice. Only an answer, which says the payment failed, is a
 - failure.
 -
 - Every account of this service shares the one LNBits wallet, so an
 - invoice this service issued is paid inside this service: at the wallet
 - both sides are the same wallet and nothing would move there.
 -
 - LNBits is asked for amounts in satoshi and answers in millisatoshi;
 - every amount, which crosses this module, is converted here so nothing
 - outside it sees a millisatoshi.
 -
 - The API keys are secrets, which are sent as a header on every call. No
 - error or log line in this module renders a request or a 'ClientError',
 - as both carry the key. The admin key, which can spend the wallet, is
 - used by one procedure only.
 -}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module OpEnergy.Account.Server.V1.Wallet.LNBits
  ( lnbitsWalletBackend
  ) where

import           Data.Int (Int64)
import           Data.Pool (Pool)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time.Clock.POSIX (POSIXTime)

import           Database.Persist
import           Database.Persist.Sql (SqlBackend, updateWhereCount)
import           Database.Persist.Postgresql (runSqlPersistMPool)
import           Data.Aeson (decode)
import           Servant.Client
                 ( BaseUrl, ClientError(..), ClientM, responseBody
                 , responseStatusCode
                 )
import           Network.HTTP.Types.Status (statusCode)

import           Data.OpEnergy.Account.API.V1.Sats (Sats(..))
import           Data.OpEnergy.Account.API.V2.Bolt11Invoice
                 ( Bolt11Invoice, everifyBolt11Invoice, unBolt11Invoice
                 )
import           Data.OpEnergy.Account.API.V2.PaymentHash
                 ( PaymentHash(..), everifyPaymentHash
                 )
import           Data.OpEnergy.Account.API.V2.WalletMode (WalletMode(..))
import           Data.OpEnergy.Account.Client (withClientEither)

import           OpEnergy.Account.Server.V1.Person (PersonId)
import           OpEnergy.Account.Server.V1.Wallet.Class
import qualified OpEnergy.Account.Server.V1.Wallet.LNBits.Client
                 as LNBits
                 ( CreateInvoiceRequest(..), DecodeRequest(..)
                 , DecodedInvoice(..), PayInvoiceRequest(..)
                 , PaymentResponse(..), PaymentStateResponse(..)
                 , RefusedPayment(..), createInvoice, decodeInvoice
                 , payInvoice, readPaymentState
                 )
import           OpEnergy.Account.Server.V1.WalletPayment
import           OpEnergy.Error
                 ( CallstackError, dbQueryError, eitherException
                 , walletAPIError, walletInvoiceAlreadyPaid
                 , walletInvoiceNotFound, walletSelfPaymentNotAllowed
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
  -> Text
  -> Pool SqlBackend
  -> WalletBackend
lnbitsWalletBackend url invoiceKey adminKey pool = WalletBackend
  { walletKind = WalletModeLNBits
  , walletCanSend = True
  , walletCanReceive = True
  , walletCreateInvoice = createInvoice url invoiceKey pool
  , walletResolveInvoice = resolveInvoice url invoiceKey pool
  , walletPayInvoice = payInvoice url invoiceKey adminKey pool
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


-- | what can be said about an invoice before paying it.
--
-- An invoice this service issued is known from its own row, as it is this
-- service, which will settle it. Anything else is read by the wallet, and
-- its amount is the wallet's answer: an amount a client sent is never what
-- is paid
resolveInvoice
  :: BaseUrl
  -> Text
  -> Pool SqlBackend
  -> PersonId
  -> Bolt11Invoice
  -> IO (Either CallstackError (Maybe ResolvedInvoice))
resolveInvoice url invoiceKey pool asker invoiceV = do
  (mours, mpaidByAsker) <- flip runSqlPersistMPool pool $ do
    ours <- selectFirst
      [ WalletPaymentInvoice ==. invoiceV
      , WalletPaymentDirection ==. Incoming
      ]
      [ Desc WalletPaymentId ]
    -- scoped to the account, which asked: what another account has paid is
    -- not something this service answers
    paidByAsker <- selectFirst
      [ WalletPaymentInvoice ==. invoiceV
      , WalletPaymentDirection ==. Outgoing
      , WalletPaymentPersonId ==. asker
      ]
      []
    return (ours, paidByAsker)
  let paidByAsker = maybe False (const True) mpaidByAsker
  case mours of
    Just (Entity _ payment) -> return $! Right $! Just $! ResolvedInvoice
      { resolvedInvoiceAmountSats = walletPaymentAmountSats payment
      , resolvedInvoicePersonId = Just (walletPaymentPersonId payment)
      , resolvedInvoicePayable = walletPaymentStatus payment == Pending
      , resolvedInvoicePaidByAsker = paidByAsker
      }
    Nothing -> do
      edecoded <- callLNBits url $ LNBits.decodeInvoice invoiceKey
        $ LNBits.DecodeRequest (unBolt11Invoice invoiceV)
      return $! fmap (Just . resolved paidByAsker) edecoded
  where
    resolved paidByAsker decoded = ResolvedInvoice
      { resolvedInvoiceAmountSats =
          msatsToSats (LNBits.decodedInvoiceAmountMsats decoded)
      , resolvedInvoicePersonId = Nothing
      -- an invoice, which somebody else has paid already, is refused by
      -- the wallet itself, which knows the chain; this service does not
      -- and does not guess
      , resolvedInvoicePayable = True
      , resolvedInvoicePaidByAsker = paidByAsker
      }

-- | pays an invoice.
--
-- An invoice of this service is settled inside it, as both sides are the
-- one LNBits wallet. Anything else is recorded as pending before the
-- wallet is asked to pay it, so a payment, which is in flight when the
-- call ends, is already written down with the hash it can be asked about
payInvoice
  :: BaseUrl
  -> Text
  -> Text
  -> Pool SqlBackend
  -> PayInvoiceParams
  -> IO (Either CallstackError PaidInvoice)
payInvoice url invoiceKey adminKey pool params = do
  mours <- flip runSqlPersistMPool pool $ selectFirst
    [ WalletPaymentInvoice ==. payInvoiceBolt11 params
    , WalletPaymentDirection ==. Incoming
    ]
    [ Desc WalletPaymentId ]
  case mours of
    Just ours -> payOwnInvoice pool params ours
    Nothing -> payOverLightning url invoiceKey adminKey pool params

-- | settles an invoice this service issued, without the wallet: the sats
-- are in the one wallet either way, so only the ledger moves. The caller
-- credits the account, which is paid, from the settled payment
payOwnInvoice
  :: Pool SqlBackend
  -> PayInvoiceParams
  -> Entity WalletPayment
  -> IO (Either CallstackError PaidInvoice)
payOwnInvoice pool params (Entity key payment)
  | walletPaymentPersonId payment == payInvoicePersonId params =
      return $! Left walletSelfPaymentNotAllowed
  | walletPaymentStatus payment /= Pending =
      return $! Left walletInvoiceNotFound
  | otherwise = do
      let now = payInvoiceNow params
      esettled <- eitherException $ flip runSqlPersistMPool pool
        $ updateWhereCount
          [ WalletPaymentId ==. key, WalletPaymentStatus ==. Pending ]
          [ WalletPaymentStatus =. Settled, WalletPaymentUpdatedAt =. now ]
      return $! case esettled of
        Left _ -> Left dbQueryError
        -- somebody settled it between the read and the write
        Right (0 :: Int64) -> Left walletInvoiceNotFound
        Right _ -> Right $! PaidInvoice
          { paidInvoicePaymentHash = walletPaymentPaymentHash payment
          , paidInvoiceAmountSats = walletPaymentAmountSats payment
          -- nothing left the wallet, so nothing was charged for it
          , paidInvoiceFeeSats = Sats 0
          , paidInvoiceProgress = PaymentSettled
          , paidInvoiceRecipientPersonId = Just (walletPaymentPersonId payment)
          }

-- | pays an invoice, which is not this service's own, over lightning
payOverLightning
  :: BaseUrl
  -> Text
  -> Text
  -> Pool SqlBackend
  -> PayInvoiceParams
  -> IO (Either CallstackError PaidInvoice)
payOverLightning url invoiceKey adminKey pool params = do
  edecoded <- callLNBits url $ LNBits.decodeInvoice invoiceKey
    $ LNBits.DecodeRequest (unBolt11Invoice (payInvoiceBolt11 params))
  case edecoded >>= readDecodedHash of
    Left err -> return $! Left err
    Right paymentHashV -> do
      erecorded <- recordPendingPayment pool params paymentHashV
      case erecorded of
        Left err -> return $! Left err
        Right () -> sendPayment url adminKey pool params paymentHashV

-- | the hash of a decoded invoice, as this service's own type
readDecodedHash
  :: LNBits.DecodedInvoice
  -> Either CallstackError PaymentHash
readDecodedHash decoded = either
  (\reason-> Left $! walletAPIError
    ("the wallet read an invoice this service can not: " <> reason))
  Right
  (everifyPaymentHash (LNBits.decodedInvoicePaymentHash decoded))

-- | writes the payment down before it is made, so one, which is in flight
-- when the call ends, is already recorded with the hash it is asked about.
-- The reserve is kept in the fee column until the real fee is known
recordPendingPayment
  :: Pool SqlBackend
  -> PayInvoiceParams
  -> PaymentHash
  -> IO (Either CallstackError ())
recordPendingPayment pool params paymentHashV = do
  let now = payInvoiceNow params
  einserted <- eitherException $ flip runSqlPersistMPool pool
    -- insertUnique, as the uniqueness of the hash and of the account's own
    -- payment of this invoice is what refuses a second attempt, which
    -- arrives while the first is still in flight
    $ insertUnique $ WalletPayment
      { walletPaymentPersonId = payInvoicePersonId params
      , walletPaymentDirection = Outgoing
      , walletPaymentStatus = Pending
      , walletPaymentAmountSats =
          maybe (Sats 0) id (payInvoiceAmountSats params)
      , walletPaymentFeeSats = payInvoiceFeeReserveSats params
      , walletPaymentPaymentHash = paymentHashV
      , walletPaymentInvoice = payInvoiceBolt11 params
      , walletPaymentNote = Nothing
      , walletPaymentExpiresAt = Nothing
      , walletPaymentCreditedAt = Nothing
      , walletPaymentCreatedAt = now
      , walletPaymentUpdatedAt = now
      }
  return $! case einserted of
    Left _ -> Left dbQueryError
    -- the account's own payment of this invoice is already recorded, by
    -- whichever attempt got here first
    Right Nothing -> Left walletInvoiceAlreadyPaid
    Right (Just _) -> Right ()

-- | asks the wallet to pay, and records what it answered
sendPayment
  :: BaseUrl
  -> Text
  -> Pool SqlBackend
  -> PayInvoiceParams
  -> PaymentHash
  -> IO (Either CallstackError PaidInvoice)
sendPayment url adminKey pool params paymentHashV = do
  let now = payInvoiceNow params
      amount = maybe (Sats 0) id (payInvoiceAmountSats params)
      reserve = payInvoiceFeeReserveSats params
      inFlight = Right $! PaidInvoice
        { paidInvoicePaymentHash = paymentHashV
        , paidInvoiceAmountSats = amount
        -- the reserve, as no fee has been charged yet. Whoever settles the
        -- payment gives back what the fee did not use
        , paidInvoiceFeeSats = reserve
        , paidInvoiceProgress = PaymentInFlight
        , paidInvoiceRecipientPersonId = Nothing
        }
  outcome <- callLNBitsOutcome url $ LNBits.payInvoice adminKey
    $ LNBits.PayInvoiceRequest (unBolt11Invoice (payInvoiceBolt11 params))
  case outcome of
    -- the wallet says it did not make the payment, so the balance goes
    -- back
    CallRefused err -> do
      _ <- recordFailed pool paymentHashV now
      return $! Left err
    -- no answer: the wallet may have taken the payment before the call
    -- failed, so it is left pending rather than failed. The scheduler asks
    -- again, and nothing gives the balance back on a payment, which may
    -- still go
    CallUnknown _ -> return inFlight
    CallAnswered answer -> case LNBits.paymentResponseStatus answer of
      "success" -> do
        let fee = msatsToSats (LNBits.paymentResponseFeeMsats answer)
        _ <- recordSettled pool paymentHashV fee now
        return $! Right $! PaidInvoice
          { paidInvoicePaymentHash = paymentHashV
          , paidInvoiceAmountSats = amount
          , paidInvoiceFeeSats = fee
          , paidInvoiceProgress = PaymentSettled
          , paidInvoiceRecipientPersonId = Nothing
          }
      "failed" -> do
        _ <- recordFailed pool paymentHashV now
        return $! Left $! walletAPIError "the wallet could not pay the invoice"
      _ -> return inFlight

-- | records a payment as made, with the fee it cost
recordSettled
  :: Pool SqlBackend
  -> PaymentHash
  -> Sats
  -> POSIXTime
  -> IO (Either Text Int64)
recordSettled pool paymentHashV fee now =
  eitherException $ flip runSqlPersistMPool pool $ updateWhereCount
    [ WalletPaymentPaymentHash ==. paymentHashV
    , WalletPaymentStatus ==. Pending
    ]
    [ WalletPaymentStatus =. Settled
    , WalletPaymentFeeSats =. fee
    , WalletPaymentUpdatedAt =. now
    ]

-- | records a payment, which the wallet says it did not make
recordFailed
  :: Pool SqlBackend
  -> PaymentHash
  -> POSIXTime
  -> IO (Either Text Int64)
recordFailed pool paymentHashV now =
  eitherException $ flip runSqlPersistMPool pool $ updateWhereCount
    [ WalletPaymentPaymentHash ==. paymentHashV
    , WalletPaymentStatus ==. Pending
    ]
    [ WalletPaymentStatus =. Failed
    , WalletPaymentUpdatedAt =. now
    ]

-- | satoshi of a millisatoshi amount, rounded away from zero, so an
-- amount, which is not a whole satoshi, is never reported as less than it
-- is. LNBits reports what left the wallet as negative
msatsToSats :: Int64 -> Sats
msatsToSats msats = Sats (fromIntegral ((abs msats + 999) `div` 1000))

-- | what became of a call, for a caller, which has to tell the wallet
-- refusing a payment from not hearing back about one
data CallOutcome r
  = CallAnswered r
  | CallRefused CallstackError
    -- ^ the wallet refused the request, so nothing happened
  | CallUnknown CallstackError
    -- ^ no answer, so whether it happened is not known

-- | performs one call, saying whether a failure was a refusal. Used by
-- the paying path, where the two can not be treated alike
callLNBitsOutcome :: BaseUrl -> ClientM r -> IO (CallOutcome r)
callLNBitsOutcome url call = do
  eresult <- eitherException $ withClientEither url call
  return $! case eresult of
    Left _ -> CallUnknown $! walletAPIError "the wallet could not be reached"
    Right (Left clientError)
      | refusalSaysNotPaid clientError ->
          CallRefused $! walletAPIError (describeClientError clientError)
      | otherwise ->
          CallUnknown $! walletAPIError (describeClientError clientError)
    Right (Right result) -> CallAnswered result

-- | whether a failed call is the wallet saying it did not make the
-- payment, rather than one, which may have been made anyway.
--
-- LNBits refuses a payment it can not make with a status of its own --
-- 520 for a balance, which does not cover it -- and says so in the body,
-- which is read here rather than guessed at from the status code. A
-- refusal before the request was acted on at all, which is what a 4xx is,
-- says the same thing
refusalSaysNotPaid :: ClientError -> Bool
refusalSaysNotPaid (FailureResponse _request response) =
  bodySaysFailed || requestWasRefused
  where
    code = statusCode (responseStatusCode response)
    requestWasRefused = code >= 400 && code < 500
    bodySaysFailed = case decode (responseBody response) of
      Just refused -> LNBits.refusedPaymentStatus refused == "failed"
      Nothing -> False
refusalSaysNotPaid _ = False

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
      | LNBits.paymentStateResponsePaid answer = settled answer
      | otherwise = case LNBits.paymentStateResponseStatus answer of
          "success" -> settled answer
          "failed" -> ObservedFailed
          _ -> ObservedPending
    settled answer =
      ObservedSettled (msatsToSats (LNBits.paymentStateResponseFeeMsats answer))

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
