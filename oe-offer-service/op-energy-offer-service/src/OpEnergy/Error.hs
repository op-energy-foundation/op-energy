{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
-- | Local copy of oe-account-service's OpEnergy.Error, adapted to this
-- service's own error vocabulary. Kept as a straight port of that module's
-- shape (CallstackError/eitherThrowJSON/runExceptPrefixT) rather than a
-- shared library between the two services: neither op-energy-api nor
-- op-energy-account-api currently exposes this as reusable, and inventing
-- a new shared package for one small module was judged not worth it for
-- this port -- see docs/plans/post-offer-api.md's "known gaps".
module OpEnergy.Error
  ( eitherThrowJSON
  , runExceptPrefixT
  , CallstackError
  , describeError

  , unspecified
  , dbQueryError

  , accountServiceUnavailable

  , authenticationFailure
  , invalidRequest
  , insufficientBalance
  , offerNotFound
  , notOfferOwner
  , offerNotOpen
  ) where

import           Data.Text(Text)
import qualified Data.Text as Text

import           Control.Monad.Error.Class(MonadError)
import           Control.Monad.Trans.Except(ExceptT, runExceptT)

import           Servant(ServerError, err400, err401, err403, err404, err409, err500, err502)
import           Data.Text.Show( tshow)
import           Data.OpEnergy.API.V1.Error(throwJSON)

data BadRequestError
  = AuthenticationFailure
  | InvalidRequest Text
  | InsufficientBalance
  | OfferNotFound
  | NotOfferOwner
  | OfferNotOpen
instance Show BadRequestError where
  show AuthenticationFailure = "authentication failure"
  show (InvalidRequest reason) = Text.unpack reason
  show InsufficientBalance = "insufficient balance"
  show OfferNotFound = "offer not found"
  show NotOfferOwner = "only the offer's creator may do this"
  show OfferNotOpen = "offer is not open"

data InternalError
  = Unspecified Text
  | DBQueryError
instance Show InternalError where
  show DBQueryError = "DB query failed"
  show (Unspecified description) = "Internal error: " ++ Text.unpack description

data Error
  = BadRequest BadRequestError
  | Internal InternalError
  | AccountServiceUnavailable Text

data CallstackError = CallstackError Text Error

unspecified :: Text -> CallstackError
unspecified = CallstackError "" . Internal . Unspecified
dbQueryError :: CallstackError
dbQueryError = CallstackError "" $! Internal DBQueryError

-- | the cross-service call to oe-account-service (Data.OpEnergy.Account.Client,
-- via OpEnergy.Offer.Server.V1.AccountClient) failed outright -- network
-- error, timeout, oe-account-service down -- as opposed to a clean 401 from
-- it (which is 'authenticationFailure' below, an unknown token, not an
-- unavailable service).
accountServiceUnavailable :: Text -> CallstackError
accountServiceUnavailable = CallstackError "" . AccountServiceUnavailable

authenticationFailure :: CallstackError
authenticationFailure = CallstackError "" $! BadRequest AuthenticationFailure
invalidRequest :: Text -> CallstackError
invalidRequest = CallstackError "" . BadRequest . InvalidRequest
insufficientBalance :: CallstackError
insufficientBalance = CallstackError "" $! BadRequest InsufficientBalance
offerNotFound :: CallstackError
offerNotFound = CallstackError "" $! BadRequest OfferNotFound
notOfferOwner :: CallstackError
notOfferOwner = CallstackError "" $! BadRequest NotOfferOwner
offerNotOpen :: CallstackError
offerNotOpen = CallstackError "" $! BadRequest OfferNotOpen

-- | converts Error into printable version
errorToServerError :: Error -> (ServerError, Text)
errorToServerError (BadRequest AuthenticationFailure) = (err401, tshow AuthenticationFailure)
errorToServerError (BadRequest NotOfferOwner) = (err403, tshow NotOfferOwner)
errorToServerError (BadRequest OfferNotFound) = (err404, tshow OfferNotFound)
errorToServerError (BadRequest OfferNotOpen) = (err409, tshow OfferNotOpen)
errorToServerError (BadRequest specificError) = (err400, tshow specificError)
errorToServerError (Internal specificError) = (err500, tshow specificError)
errorToServerError (AccountServiceUnavailable reason) = (err502, "account service unavailable: " <> reason)

-- | renders a CallstackError as plain text, for callers that need to log
-- one themselves rather than throw it as a response -- e.g.
-- OpEnergy.Offer.Server.V1.OfferService.refundAndCloseOffer, which reports
-- a failed credit as a log line, not an HTTP error (the request it's
-- handling already succeeded by the time it happens).
describeError :: CallstackError -> Text
describeError (CallstackError callstack err) =
  let (_, reason) = errorToServerError err
  in callstack <> ": ERROR: " <> reason

-- | The goal of this function is to turn failure results from @payload@ into JSON  `ServerError`s.
-- For this, @payload@ should return value of type @Either l r@: In the case that @payload@
-- returns @Left reason@, this function will call @handler reason@ and will
-- throw a JSON exception with 'throwJSON' function
-- In the case of 'Right result' it will just return the result.
eitherThrowJSON
  :: ( Monad m
     , MonadError ServerError m
     )
  => (Text -> m ())
  -> m (Either CallstackError r)
  -> m r
eitherThrowJSON handler payload = do
  eret <- payload
  case eret of
    Right ret -> return ret
    Left (CallstackError callstack err) -> do
      let
          !(serverError, reason) = errorToServerError err
          !msg = callstack <> ": ERROR: " <> reason
      handler msg
      throwJSON serverError msg

-- | The goal of this function is to add prefix to the error reason
-- example
-- ```haskell
-- runExceptPrefixT "MyFunction" $ throwE "error" -- will return @Left "MyFunction: error"@
-- ```
runExceptPrefixT
  :: Monad m
  => Text
  -> ExceptT CallstackError m r
  -> m (Either CallstackError r)
runExceptPrefixT prefix payload = do
  eret <- runExceptT payload
  return $! either
    (\(CallstackError callstack err)-> Left
      (CallstackError (prefix <> "." <> callstack) err)
    )
    Right
    eret
