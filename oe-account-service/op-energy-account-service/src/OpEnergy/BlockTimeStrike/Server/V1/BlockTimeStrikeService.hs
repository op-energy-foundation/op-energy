{-- | This module implements BlockTime strike service.
 -}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards          #-}
module OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeService
  ( getBlockTimeStrikeFuture
  , createBlockTimeStrikeFuture
  , getBlockTimeStrikePast
  ) where

import           Servant (err404, throwError)
import           Control.Monad.Trans.Reader (ask)
import           Control.Monad.IO.Class (liftIO, MonadIO)
import qualified Data.List as List
import           Control.Monad.Logger(logError)
import           Data.Time.Clock(getCurrentTime)
import           Data.Time.Clock.POSIX(utcTimeToPOSIXSeconds)
import qualified Control.Concurrent.STM.TVar as TVar

import           Database.Persist.Postgresql
import           Prometheus(MonadMonitor)
import qualified Prometheus as P


import           Data.OpEnergy.Account.API.V1.Account
import           Data.OpEnergy.API.V1.Block
import           Data.OpEnergy.API.V1.Natural
import           Data.OpEnergy.API.V1.Positive(naturalFromPositive)
import           Data.OpEnergy.Account.API.V1.BlockTimeStrike
import           OpEnergy.Account.Server.V1.Config (Config(..))
import           OpEnergy.Account.Server.V1.Class (AppT, AppM, State(..), runLogging)
import qualified OpEnergy.BlockTimeStrike.Server.V1.Class as BlockTime
import qualified OpEnergy.Account.Server.V1.Metrics as Metrics(MetricsState(..))
import           OpEnergy.Account.Server.V1.AccountService (mgetPersonByAccountToken)

-- | O(ln users) + O(strike future)
-- returns list BlockTimeStrikeFuture records. requires authenticated user
getBlockTimeStrikeFuture :: AccountToken-> AppM [BlockTimeStrikeFuture ]
getBlockTimeStrikeFuture token = do
  mperson <- mgetPersonByAccountToken token
  case mperson of
    Nothing -> throwError err404
    Just person -> getBlockTimeStrikeFuture person
  where
    getBlockTimeStrikeFuture :: (MonadIO m, MonadMonitor m) => Entity Person -> AppT m [BlockTimeStrikeFuture]
    getBlockTimeStrikeFuture (Entity _ _) = do
      State{ accountDBPool = pool
           , metrics = Metrics.MetricsState { Metrics.getBlockTimeStrikeFuture = getBlockTimeStrikeFuture
                                            }
           } <- ask
      P.observeDuration getBlockTimeStrikeFuture $ do
        liftIO $ flip runSqlPersistMPool pool $ selectList [] [] >>= return . List.map (\(Entity _ blocktimeStrikeFuture) -> blocktimeStrikeFuture)

-- | O(ln accounts).
-- Tries to create future block time strike. Requires authenticated user and blockheight should be in the future
createBlockTimeStrikeFuture :: AccountToken-> BlockHeight-> Natural Int-> AppM ()
createBlockTimeStrikeFuture token blockHeight nlocktime = do
  State{ config = Config{ configBlockTimeStrikeMinimumBlockAheadCurrentTip = configBlockTimeStrikeMinimumBlockAheadCurrentTip}
       , blockTimeState = BlockTime.State{ currentTip = currentTipV }
       } <- ask
  mcurrentTip <- liftIO $ TVar.readTVarIO currentTipV
  case mcurrentTip of
    Nothing -> do
      let msg = "ERROR: there is no current tip yet"
      runLogging $ $(logError) msg
      throwError err404
    Just tip
      | blockHeaderHeight tip + naturalFromPositive configBlockTimeStrikeMinimumBlockAheadCurrentTip < blockHeight -> do
        let msg = "ERROR: block height for new block time strike should be in the future + minimum configBlockTimeStrikeMinimumBlockAheadCurrentTip"
        runLogging $ $(logError) msg
        throwError err404
    _ -> do
      mperson <- mgetPersonByAccountToken token
      case mperson of
        Nothing -> do
          let msg = "ERROR: person was not able to authenticate itself"
          runLogging $ $(logError) msg
          throwError err404
        Just person -> createBlockTimeStrikeFutureEsnuredConditions person
  where
    createBlockTimeStrikeFutureEsnuredConditions :: (MonadIO m, MonadMonitor m) => (Entity Person) -> AppT m ()
    createBlockTimeStrikeFutureEsnuredConditions _ = do
      State{ accountDBPool = pool
           , metrics = Metrics.MetricsState { Metrics.createBlockTimeStrikeFuture = createBlockTimeStrikeFuture
                                            }
           } <- ask
      P.observeDuration createBlockTimeStrikeFuture $ do
        nowUTC <- liftIO getCurrentTime
        let now = utcTimeToPOSIXSeconds nowUTC
        liftIO $ flip runSqlPersistMPool pool $ insert_ $! BlockTimeStrikeFuture
          { blockTimeStrikeFutureBlock = blockHeight
          , blockTimeStrikeFutureNlocktime = fromIntegral nlocktime
          , blockTimeStrikeFutureCreationTime = now
          }

-- | O(ln accounts) + O(BlockTimeStrikePast).
-- returns list BlockTimeStrikePast records. requires authenticated user
getBlockTimeStrikePast :: AccountToken-> AppM [ BlockTimeStrikePast]
getBlockTimeStrikePast token = do
  mperson <- mgetPersonByAccountToken token
  case mperson of
    Nothing -> do
      let msg = "ERROR: person was not able to authenticate itself"
      runLogging $ $(logError) msg
      throwError err404
    Just person -> getBlockTimeStrikePast person
  where
    getBlockTimeStrikePast :: (MonadIO m, MonadMonitor m) => (Entity Person) -> AppT m [ BlockTimeStrikePast]
    getBlockTimeStrikePast _ = do
      State{ accountDBPool = pool
           , metrics = Metrics.MetricsState { Metrics.getBlockTimeStrikePast = getBlockTimeStrikePast
                                            }
           } <- ask
      P.observeDuration getBlockTimeStrikePast $ do
        liftIO $ flip runSqlPersistMPool pool $ selectList [] [] >>= return . List.map (\(Entity _ blocktimeStrikeFuture) -> blocktimeStrikeFuture)
