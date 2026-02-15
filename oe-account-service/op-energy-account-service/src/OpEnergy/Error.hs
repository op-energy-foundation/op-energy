{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module OpEnergy.Error
  ( eitherThrowJSON
  , runExceptPrefixT
  , eitherException
  ) where

import           Data.Text(Text)
import qualified Data.Text as Text

import           Data.Aeson (ToJSON)
import           Control.Monad.Error.Class(MonadError)
import           Control.Monad.Trans.Except(ExceptT, runExceptT)
import           Control.Exception.Safe (SomeException)
import qualified Control.Exception.Safe as E

import           Servant(ServerError)
import           Data.OpEnergy.API.V1.Error(throwJSON)

-- | The goal of this function is to turn failure results from @payload@ into JSON  `ServerError`s.
-- For this, @payload@ should return value of type @Either l r@: In the case that @payload@
-- returns @Left reason@, this function will call @handler reason@ and will
-- throw a JSON exception with 'throwJSON' function
-- In the case of 'Right result' it will just return the result.
eitherThrowJSON
  :: ( Monad m
     , MonadError ServerError m
     , ToJSON l
     )
  => (l -> m (ServerError, Text))
  -> m (Either l r)
  -> m r
eitherThrowJSON handler payload = do
  eret <- payload
  case eret of
    Right ret -> return ret
    Left reason -> do
      (err, msg) <- handler reason
      throwJSON err msg


-- | The goal of this function is to add prefix to the error reason
-- example
-- ```haskell
-- runExceptPrefixT "MyFunction" $ throwE "error" -- will return @Left "MyFunction: error"@
-- ```
runExceptPrefixT
  :: Monad m
  => Text
  -> ExceptT Text m r
  -> m (Either Text r)
runExceptPrefixT prefix payload = do
  eret <- runExceptT payload
  return $! either
    (\reason-> Left (prefix <> ": " <> reason))
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

