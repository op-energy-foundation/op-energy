-- | This module's main purpose is to contain a scheduled routine to create future time strikes automatically.
{-# LANGUAGE TemplateHaskell          #-}
module OpEnergy.BlockTimeStrike.Server.V1.BlockTimeScheduledFutureStrikeCreation
  ( maybeCreateFutureStrikes
  ) where

import qualified Data.List as List
import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Control.Monad.Reader(ask)
import Control.Monad.IO.Class(MonadIO, liftIO)
import           Control.Monad.Logger( logInfo, logDebug)
import Control.Monad(forM_, when)

import           Database.Persist.Postgresql
import           Prometheus(MonadMonitor)
import qualified Prometheus as P

import           Data.OpEnergy.API.V1.Block
import           Data.OpEnergy.Account.API.V1.BlockTimeStrike
import           Data.OpEnergy.API.V1.Positive
import           Data.OpEnergy.API.V1.Natural
import           OpEnergy.Account.Server.V1.Class (AppT, State(..), runLogging)
import           OpEnergy.Account.Server.V1.Config
import qualified OpEnergy.Account.Server.V1.Metrics as Metrics(MetricsState(..))
import           Data.Text.Show

-- | This function is being called when latest confirmed block had been discovered. You should expect, that it can be called by recieving notification from the blockspan service either at discover or at a time of connection of block time service to blockspan service. So it is expected, that it will run at each restart of the blockspan service or at reconnection to blockspan service or at blocktime service restart.
maybeCreateFutureStrikes
  :: ( MonadIO m
     , MonadMonitor m
     )
  => BlockHeader
  -> AppT m ()
maybeCreateFutureStrikes currentTip = do
  ensureFutureStrikeExistsAhead currentTip

-- | this function creates necessary future strikes if they haven't been created yet as it tries to be idempotent.
-- NOTE: currently, we use CurrentTip + Config.configBlockTimeStrikeFutureGuessMinimumBlockAheadCurrentTip as the minimum block height for a future strike.
ensureFutureStrikeExistsAhead
  :: ( MonadIO m
     , MonadMonitor m
     )
  => BlockHeader
  -> AppT m ()
ensureFutureStrikeExistsAhead currentTip = do
  State{ config = Config
         { configBlockTimeFutureStrikeShouldExistsAheadCurrentTip = configBlockTimeFutureStrikeShouldExistsAheadCurrentTip
         , configBlockTimeStrikeFutureGuessMinimumBlockAheadCurrentTip = configBlockTimeStrikeFutureGuessMinimumBlockAheadCurrentTip
         }
       , accountDBPool = pool
       , metrics = Metrics.MetricsState { Metrics.ensureFutureStrikeExistsAhead = ensureFutureStrikeExistsAhead
                                        }
       } <- ask
  P.observeDuration ensureFutureStrikeExistsAhead $ do
    let minimumFutureStrikeHeight = blockHeaderHeight currentTip + naturalFromPositive configBlockTimeStrikeFutureGuessMinimumBlockAheadCurrentTip
        maximumFutureStrikeHeight = blockHeaderHeight currentTip + naturalFromPositive configBlockTimeFutureStrikeShouldExistsAheadCurrentTip
    runLogging $ $(logDebug) ("ensureFutureStrikeExistsAhead: currentTip: " <> tshow currentTip <>  ", min/max: " <> tshow [minimumFutureStrikeHeight, maximumFutureStrikeHeight])
    nonExistentBlockHeights <- liftIO $ do
      now <- getCurrentTime
      flip runSqlPersistMPool pool $ do
        futureStrikesE <- selectList
          [ BlockTimeStrikeFutureBlock >=. minimumFutureStrikeHeight
          , BlockTimeStrikeFutureBlock <=. maximumFutureStrikeHeight
          ] []
        let futureStrikes = List.map (\(Entity _ strike) -> blockTimeStrikeFutureBlock strike) futureStrikesE
        let nonExistentStrikeHeights = List.filter
              (\height-> height `notElem` futureStrikes)
              [ minimumFutureStrikeHeight .. maximumFutureStrikeHeight ]
        forM_ nonExistentStrikeHeights $ \height-> do
          insert (BlockTimeStrikeFuture { blockTimeStrikeFutureBlock = height
                                        , blockTimeStrikeFutureNlocktime =
                                          fromIntegral
                                          $ blockHeaderMediantime currentTip
                                          + fromIntegral ((fromNatural (height - blockHeaderHeight currentTip)) * 600) -- assume 10 minutes time slices for nlocktime
                                        , blockTimeStrikeFutureCreationTime = utcTimeToPOSIXSeconds now
                                        })

        return nonExistentStrikeHeights
    when (List.length nonExistentBlockHeights > 0) $ do
      runLogging $ $(logInfo) ("ensureFutureStrikeExistsAhead: created future strikes: " <> (tshow nonExistentBlockHeights))

