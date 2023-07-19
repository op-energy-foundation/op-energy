{--
 - This module defines data type that keep all the state, used by backend
 -}
module OpEnergy.BlockTimeStrike.Server.V1.Class where

import           Control.Concurrent.STM.TVar (TVar)
import qualified Control.Concurrent.STM.TVar as TVar
import           Control.Monad.IO.Class (MonadIO, liftIO)

import           Data.OpEnergy.API.V1.Block ( BlockHeader)
import           OpEnergy.Account.Server.V1.Config

-- | defines the whole state used by backend
data State = State
  { currentTip :: TVar (Maybe BlockHeader)
  }

-- | constructs default state with given config and DB pool
defaultState :: MonadIO m => Config-> m State
defaultState _ = do
  _currentTip <- liftIO $ TVar.newTVarIO Nothing
  return $ State
    { currentTip = _currentTip -- websockets' init data relies on whole BlockHeader
    }
