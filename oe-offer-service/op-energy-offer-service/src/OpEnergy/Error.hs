{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
-- | Local copy of oe-account-service's OpEnergy.Error, adapted to this
-- service's own error vocabulary.
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

-- | renders a CallstackError as plain text for logging
describeError :: CallstackError -> Text
describeError (CallstackError callstack err) =
  let (_, reason) = errorToServerError err
  in callstack <> ": ERROR: " <> reason

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
