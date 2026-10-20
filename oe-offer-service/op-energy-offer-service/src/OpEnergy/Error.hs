{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
-- | Local copy of oe-account-service's OpEnergy.Error, adapted to this
-- service's own error vocabulary.
module OpEnergy.Error
  ( eitherThrowJSON
  , runExceptPrefixT
  , exceptTMaybeT
  , CallstackError
  , describeError

  , unspecified
  , dbQueryError
  , potDoesNotCoverFee

  , accountServiceUnavailable
  , accountServiceRefused
  , isAccountServiceRefused
  , accountUnknown
  , isAccountUnknown
  , blockspanRequestFailed
  , chainTipUnknown

  , authenticationFailure
  , invalidRequest
  , insufficientBalance
  , offerNotFound
  , notOfferOwner
  , offerNotOpen
  , offerFilled
  , offerExpired
  , offerChanged
  , cannotAcceptOwnOffer
  , catchBadRequest
  ) where

import           Data.Text(Text)
import qualified Data.Text as Text
import           Data.Word(Word64)

import           Control.Monad.Error.Class(MonadError)
import           Control.Monad.Trans.Except(ExceptT(..), runExceptT, throwE)

import           Servant
                   ( ServerError, err400, err401, err403, err404, err409, err500
                   , err502, err503
                   )
import           Data.Text.Show( tshow)
import           Data.OpEnergy.API.V1.Error(throwJSON)

data BadRequestError
  = AuthenticationFailure
  | InvalidRequest Text
  | InsufficientBalance
  | OfferNotFound
  | NotOfferOwner
  | OfferNotOpen
  | OfferFilled
  | OfferExpired
  | OfferChanged
  | CannotAcceptOwnOffer
instance Show BadRequestError where
  show AuthenticationFailure = "authentication failure"
  show (InvalidRequest reason) = Text.unpack reason
  show InsufficientBalance = "insufficient balance"
  show OfferNotFound = "offer not found"
  show NotOfferOwner = "only the offer's creator may do this"
  show OfferNotOpen = "offer is not open"
  show OfferFilled = "offer has no remaining contracts"
  show OfferExpired = "offer has expired at the current chain tip"
  show OfferChanged = "offer has changed meanwhile, try again"
  show CannotAcceptOwnOffer = "cannot accept your own offer"

data InternalError
  = Unspecified Text
  | DBQueryError
  | PotDoesNotCoverFee Word64 Word64
    -- ^ pot of a contract and the platform fee, both in sats
instance Show InternalError where
  show DBQueryError = "DB query failed"
  show (PotDoesNotCoverFee potSats feeSats) =
    "contract pot of " ++ show potSats ++ " sats does not cover the platform fee of "
    ++ show feeSats ++ " sats"
  show (Unspecified description) = "Internal error: " ++ Text.unpack description

data Error
  = BadRequest BadRequestError
  | Internal InternalError
  | AccountServiceUnavailable Text
    -- ^ the account service could not be reached, so it is not known
    -- whether it has carried out the request
  | AccountServiceRefused Text
    -- ^ the account service has answered the request with an error, so it
    -- has not carried it out. The same request may be carried out later:
    -- what it was refused for, a secret this service holds wrongly, is
    -- put right where the services are configured and not here
  | AccountUnknown Text
    -- ^ the account service does not have the account the request names,
    -- so it has carried nothing out and no later attempt of the same
    -- request can succeed either
  | BlockspanRequestFailed Text
  | ChainTipUnknown
    -- ^ the service has not received the chain tip yet

data CallstackError = CallstackError Text Error

unspecified :: Text -> CallstackError
unspecified = CallstackError "" . Internal . Unspecified
dbQueryError :: CallstackError
dbQueryError = CallstackError "" $! Internal DBQueryError
potDoesNotCoverFee :: Word64 -> Word64 -> CallstackError
potDoesNotCoverFee potSats feeSats = CallstackError "" $! Internal $! PotDoesNotCoverFee potSats feeSats

accountServiceUnavailable :: Text -> CallstackError
accountServiceUnavailable = CallstackError "" . AccountServiceUnavailable
accountServiceRefused :: Text -> CallstackError
accountServiceRefused = CallstackError "" . AccountServiceRefused
accountUnknown :: Text -> CallstackError
accountUnknown = CallstackError "" . AccountUnknown

-- | 'True' for the error of a request, which the account service itself has
-- answered with an error: it has carried nothing out, so its caller may
-- repeat the request. Any other error, a lost connection above all, leaves
-- the outcome of the request unknown, so repeating it could carry it out a
-- second time.
--
-- Example: the settlement sweep takes a settlement back and settles the
-- contract again on a later tick when its payout was refused, while a
-- payout of an unknown outcome is logged for reconciliation instead.
isAccountServiceRefused :: CallstackError -> Bool
isAccountServiceRefused (CallstackError _ (AccountServiceRefused _)) = True
isAccountServiceRefused _ = False

-- | 'True' for the error of a request naming an account, which the account
-- service does not have. Nothing was carried out and nothing will be: a
-- caller, which repeats a refused request on a later tick, has to stop
-- for this one rather than repeat it for good.
--
-- Example: the settlement sweep leaves a contract settled, with its winner
-- unpaid, when the winner's account is unknown, rather than taking the
-- settlement back for every later tick to try the same payout again.
isAccountUnknown :: CallstackError -> Bool
isAccountUnknown (CallstackError _ (AccountUnknown _)) = True
isAccountUnknown _ = False
blockspanRequestFailed :: Text -> CallstackError
blockspanRequestFailed = CallstackError "" . BlockspanRequestFailed
chainTipUnknown :: CallstackError
chainTipUnknown = CallstackError "" $! ChainTipUnknown

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
offerFilled :: CallstackError
offerFilled = CallstackError "" $! BadRequest OfferFilled
offerExpired :: CallstackError
offerExpired = CallstackError "" $! BadRequest OfferExpired
offerChanged :: CallstackError
offerChanged = CallstackError "" $! BadRequest OfferChanged
cannotAcceptOwnOffer :: CallstackError
cannotAcceptOwnOffer = CallstackError "" $! BadRequest CannotAcceptOwnOffer

-- | converts Error into printable version
errorToServerError :: Error -> (ServerError, Text)
errorToServerError (BadRequest AuthenticationFailure) = (err401, tshow AuthenticationFailure)
errorToServerError (BadRequest NotOfferOwner) = (err403, tshow NotOfferOwner)
errorToServerError (BadRequest OfferNotFound) = (err404, tshow OfferNotFound)
errorToServerError (BadRequest OfferNotOpen) = (err409, tshow OfferNotOpen)
errorToServerError (BadRequest OfferFilled) = (err409, tshow OfferFilled)
errorToServerError (BadRequest OfferExpired) = (err409, tshow OfferExpired)
errorToServerError (BadRequest OfferChanged) = (err409, tshow OfferChanged)
errorToServerError (BadRequest CannotAcceptOwnOffer) = (err403, tshow CannotAcceptOwnOffer)
errorToServerError (BadRequest specificError) = (err400, tshow specificError)
errorToServerError (Internal specificError) = (err500, tshow specificError)
errorToServerError (AccountServiceUnavailable reason) = (err502, "account service unavailable: " <> reason)
errorToServerError (AccountServiceRefused reason) = (err502, "account service refused the request: " <> reason)
errorToServerError (AccountUnknown reason) = (err502, "account service does not have the account: " <> reason)
errorToServerError (BlockspanRequestFailed reason) = (err502, "blockspan request failed: " <> reason)
errorToServerError ChainTipUnknown =
  (err503, "the chain tip is not known yet, try again shortly")

-- | renders a CallstackError as plain text for logging
describeError :: CallstackError -> Text
describeError (CallstackError callstack err) =
  let (_, reason) = errorToServerError err
  in callstack <> ": ERROR: " <> reason

-- | Unwrap a @Maybe@ from a monadic action into @ExceptT@,
-- throwing the given error on @Nothing@.
exceptTMaybeT
  :: Monad m
  => CallstackError
  -> m (Maybe r)
  -> ExceptT CallstackError m r
exceptTMaybeT err action = do
  mval <- ExceptT $ fmap Right action
  case mval of
    Nothing -> throwE err
    Just v  -> return v

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

-- | runs @payload@. If @payload@ throws BadRequest error, will catch it and
-- will return such error as a value (ie,
-- `Right (Left (CallstackError callstack (BadRequest ...)))`
-- any other error returned as error `(Left ...)`
-- the success value will be returned as `Right (Right successValue))`
catchBadRequest
  :: Monad m
  => m (Either CallstackError r)
  -> m (Either CallstackError (Either CallstackError r))
catchBadRequest payload = do
  eresult <- payload
  case eresult of
    Left (CallstackError _callstack (BadRequest _specificError )) ->
      returnExceptionAsValue eresult
      where
        returnExceptionAsValue = return . Right
    Left nonCatchableError -> return (Left nonCatchableError)
    Right _successValue -> return (Right (Right _successValue))
