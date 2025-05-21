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


