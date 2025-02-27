{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module OpEnergy.Error
  ( eitherThrowJSON
  ) where

import           Data.Text(Text)
import           Data.Aeson (ToJSON)
import           Control.Monad.Error.Class(MonadError)

import           Servant(ServerError)
import           Data.OpEnergy.API.V1.Error(throwJSON)

-- | the goal of this function is to wrap the 'payload' with exception handling
-- For this, payload should return value of type Either l r: In case if payload
-- will return 'Left reason', this function will call 'handler reason' and will
-- throw JSON exception with 'throwJSON' function
-- In case of 'Right result' it will just return the result
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


