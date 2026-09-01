{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module OpEnergy.Error
  ( eitherThrowJSON
  , runExceptPrefixT
  , eitherException
  , CallstackError

  , unspecified
  , latestUnconfirmedBlockHeightMissing
  , latestConfirmedBlockMissing
  , dbQueryError

  , blockspanAPIError

  , authenticationFailure
  , accountNotFound
  , passwordNotSet
  , invalidCredentials
  , displayNameAlreadyTaken
  , strikeNotFound
  , strikeMediantimeShouldBeInFuture
  , blockHeightShouldBeInFuture
  , calculatedGuessesCountNotFound
  ) where

import           Data.Text(Text)
import qualified Data.Text as Text

import           Control.Monad.Error.Class(MonadError)
import           Control.Monad.Trans.Except(ExceptT, runExceptT)
import           Control.Exception.Safe (SomeException)
import qualified Control.Exception.Safe as E

import           Servant(ServerError, err400, err500)
import           Data.Text.Show( tshow)
import           Data.OpEnergy.API.V1.Error(throwJSON)

data BadRequestError
  = AuthenticationFailure
  | AccountNotFound
  | PasswordNotSet
  | InvalidCredentials
  | DisplayNameAlreadyTaken
  | StrikeNotFound
  | StrikeMediantimeShouldBeInFuture
  | BlockHeightShouldBeInFuture
  | CalculatedGuessesCountNotFound
instance Show BadRequestError where
  show CalculatedGuessesCountNotFound = "calculated guesses count not found"
  show BlockHeightShouldBeInFuture = "strike's block height should be in the future"
  show StrikeMediantimeShouldBeInFuture = "strikeMediantime should be in the future"
  show StrikeNotFound = "strike not found"
  show AuthenticationFailure = "authentication failure"
  show AccountNotFound = "account not found with given token"
  show PasswordNotSet = "account has no password set"
  show InvalidCredentials = "invalid credentials"
  show DisplayNameAlreadyTaken = "display name already taken"

data InternalError
  = Unspecified Text
  | LatestUnconfirmedBlockHeightMissing
  | LatestConfirmedBlockMissing
  | DBQueryError
instance Show InternalError where
  show DBQueryError = "DB query failed"
  show LatestConfirmedBlockMissing = "latest confirmed block hasn't been received yet"
  show LatestUnconfirmedBlockHeightMissing = "latest unconfirmed block hasn't been received yet"
  show (Unspecified description) = "Internal error: " ++ Text.unpack description

data Error
  = BadRequest BadRequestError
  | Internal InternalError
  | BlockspanAPI Text

data CallstackError = CallstackError Text Error

unspecified :: Text -> CallstackError
unspecified = CallstackError "" . Internal . Unspecified
latestUnconfirmedBlockHeightMissing :: CallstackError
latestUnconfirmedBlockHeightMissing = CallstackError "" $! Internal LatestUnconfirmedBlockHeightMissing
latestConfirmedBlockMissing :: CallstackError
latestConfirmedBlockMissing = CallstackError "" $! Internal LatestConfirmedBlockMissing
dbQueryError :: CallstackError
dbQueryError = CallstackError "" $! Internal DBQueryError

blockspanAPIError :: Text-> CallstackError
blockspanAPIError = CallstackError "" . BlockspanAPI

authenticationFailure :: CallstackError
authenticationFailure = CallstackError "" $! BadRequest AuthenticationFailure
accountNotFound :: CallstackError
accountNotFound = CallstackError "" $! BadRequest AccountNotFound
passwordNotSet :: CallstackError
passwordNotSet = CallstackError "" $! BadRequest PasswordNotSet
invalidCredentials :: CallstackError
invalidCredentials = CallstackError "" $! BadRequest InvalidCredentials
displayNameAlreadyTaken :: CallstackError
displayNameAlreadyTaken = CallstackError "" $! BadRequest DisplayNameAlreadyTaken
strikeNotFound :: CallstackError
strikeNotFound = CallstackError "" $! BadRequest StrikeNotFound
strikeMediantimeShouldBeInFuture :: CallstackError
strikeMediantimeShouldBeInFuture = CallstackError "" $! BadRequest StrikeMediantimeShouldBeInFuture
blockHeightShouldBeInFuture :: CallstackError
blockHeightShouldBeInFuture = CallstackError "" $! BadRequest BlockHeightShouldBeInFuture
calculatedGuessesCountNotFound :: CallstackError
calculatedGuessesCountNotFound = CallstackError "" $! BadRequest CalculatedGuessesCountNotFound

-- | converts Error into printable version
errorToServerError :: Error -> (ServerError, Text)
errorToServerError (BadRequest specificError) = (err400, tshow specificError)
errorToServerError (Internal specificError) = (err500, tshow specificError)
errorToServerError (BlockspanAPI specificError) = (err500, tshow specificError)


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

-- | this functions's goal is to handle possible exception into @Either@ type
-- in order to wrap side-effectful routine into ExceptT transformer
-- Example:
-- @ eitherException $ readFile "/file/not/found" @
eitherException
  :: IO r
  -> IO (Either Text r)
eitherException next = do
  !ret <- E.handle
    (\(e::SomeException)->
      return (Left (Text.pack (show e)))
    )
    (do
      !ret <- next
      return (Right ret)
    )
  return ret

