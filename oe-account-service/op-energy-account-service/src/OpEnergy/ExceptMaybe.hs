module OpEnergy.ExceptMaybe
  ( exceptTMaybeT
  ) where

import           Control.Monad.Trans.Except(ExceptT(..))

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

