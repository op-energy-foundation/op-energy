{-# LANGUAGE TemplateHaskell            #-}
module OpEnergy.ExceptMaybe
  ( exceptTMaybeT
  , eitherLogThrowOrReturn
  , runExceptPrefixT
  ) where

import           Data.Text(Text)

import           Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import           Control.Monad.Logger(logError)
import           OpEnergy.Account.Server.V1.Class ( AppM, runLogging)
import           Data.OpEnergy.API.V1.Error (throwJSON)
import           Servant.Server.Internal.ServerError(err500)


-- | this function wraps function, which returns value of type 'Maybe result'
-- into ExceptT transformer by providing 'Left' value in case of function will
-- return Nothing. The case 'Just result' will be mapped to 'Right result'
exceptTMaybeT
  :: Monad m
  => l
  -> m (Maybe r)
  -> ExceptT l m r
exceptTMaybeT left f = ExceptT $ do
  v <- f
  case v of
    Nothing -> return (Left left)
    Just some -> return (Right some)

-- | In case of success this function will just return @r@
-- In case of failure, it will log error reason and throw it as a JSON error.
eitherLogThrowOrReturn
  :: AppM (Either Text r)
  -> AppM r
eitherLogThrowOrReturn foo = do
  r <- foo
  case r of
    Left reason-> do
      runLogging $ $(logError) reason
      throwJSON err500 reason
    Right ret -> return ret

-- | this function is basically an extension to @runExceptT@ with the addition
-- of @prefix@ to the error reason in case of failure
runExceptPrefixT
  :: (Monad m)
  => Text
  -> ExceptT Text m r
  -> m (Either Text r)
runExceptPrefixT prefix payload = do
  ret <- runExceptT payload
  return $! either (\reason -> Left (prefix <> ": " <> reason)) Right ret

