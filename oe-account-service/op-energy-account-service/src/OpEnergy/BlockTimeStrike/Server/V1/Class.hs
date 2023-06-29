{--
 - This module defines data type that keep all the state, used by backend
 -}
module OpEnergy.BlockTimeStrike.Server.V1.Class where

import           Control.Concurrent.STM.TVar (TVar)
import qualified Control.Concurrent.STM.TVar as TVar
import           Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as MVar
import           Control.Monad.IO.Class (MonadIO, liftIO)

import           Data.OpEnergy.API.V1.Block ( BlockHeader)
import           OpEnergy.Account.Server.V1.Config

-- | defines the whole state used by backend
data State = State
  { latestConfirmedBlock :: TVar (Maybe BlockHeader) -- this variable will be used to check limits during creation of block time strikes and/or guesses
  , blockTimeStrikeCurrentTip :: MVar BlockHeader -- this variable will be used to notify BlockTimeStrikeService.newTipHandlerLoop about new current tip
  }

-- | constructs default state with given config and DB pool
defaultState :: MonadIO m => Config-> m State
defaultState _ = do
  latestConfirmedBlock <- liftIO $ TVar.newTVarIO Nothing
  blockTimeStrikeCurrentTipV <- liftIO $ MVar.newEmptyMVar
  return $ State
    { latestConfirmedBlock = latestConfirmedBlock -- websockets' init data relies on whole BlockHeader
    , blockTimeStrikeCurrentTip = blockTimeStrikeCurrentTipV
    }
