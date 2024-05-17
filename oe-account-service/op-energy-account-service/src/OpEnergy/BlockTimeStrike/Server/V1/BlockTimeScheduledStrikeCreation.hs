-- | This module's main purpose is to contain a scheduled routine to create future time strikes automatically.
{-# LANGUAGE TemplateHaskell          #-}
module OpEnergy.BlockTimeStrike.Server.V1.BlockTimeScheduledStrikeCreation
  ( maybeCreateStrikes
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

import           Data.OpEnergy.API.V1.Block
import           Data.OpEnergy.Account.API.V1.BlockTimeStrike
import           Data.OpEnergy.API.V1.Positive
import           Data.OpEnergy.API.V1.Natural
import           OpEnergy.Account.Server.V1.Class (profile, AppT, State(..), runLogging, withDBTransaction)
import           OpEnergy.Account.Server.V1.Config
import           Data.Text.Show

-- | This function is being called when latest confirmed block had been discovered. You should expect, that it can be called by recieving notification from the blockspan service either at discover or at a time of connection of block time service to blockspan service. So it is expected, that it will run at each restart of the blockspan service or at reconnection to blockspan service or at blocktime service restart.
maybeCreateStrikes
  :: ( MonadIO m
     , MonadMonitor m
     )
  => BlockHeader
  -> AppT m ()
maybeCreateStrikes currentTip = do
  ensureStrikeExistsAhead currentTip

-- | this function creates necessary future strikes if they haven't been created yet as it tries to be idempotent.
-- NOTE: currently, we use CurrentTip + Config.configBlockTimeStrikeGuessMinimumBlockAheadCurrentTip as the minimum block height for a future strike.
ensureStrikeExistsAhead
  :: ( MonadIO m
     , MonadMonitor m
     )
  => BlockHeader
  -> AppT m ()
ensureStrikeExistsAhead currentTip = profile "ensureStrikeExistsAhead" $ do
  State{ config = Config
         { configBlockTimeStrikeShouldExistsAheadCurrentTip = configBlockTimeStrikeShouldExistsAheadCurrentTip
         , configBlockTimeStrikeGuessMinimumBlockAheadCurrentTip = configBlockTimeStrikeGuessMinimumBlockAheadCurrentTip
         }
       } <- ask
  let minimumStrikeHeight = blockHeaderHeight currentTip + naturalFromPositive configBlockTimeStrikeGuessMinimumBlockAheadCurrentTip
      maximumStrikeHeight = blockHeaderHeight currentTip + naturalFromPositive configBlockTimeStrikeShouldExistsAheadCurrentTip
  runLogging $ $(logDebug) ("ensureStrikeExistsAhead: currentTip: " <> tshow currentTip <>  ", min/max: " <> tshow [minimumStrikeHeight, maximumStrikeHeight])
  nonExistentBlockHeights <- do
    now <- liftIO getCurrentTime
    withDBTransaction "" $ do
      futureStrikesE <- selectList
        [ BlockTimeStrikeBlock >=. minimumStrikeHeight
        , BlockTimeStrikeBlock <=. maximumStrikeHeight
        ] []
      let futureStrikes = List.map (\(Entity _ strike) -> blockTimeStrikeBlock strike) futureStrikesE
      let nonExistentStrikeHeights = List.filter
            (\height-> height `notElem` futureStrikes)
            [ minimumStrikeHeight .. maximumStrikeHeight ]
      forM_ nonExistentStrikeHeights $ \height-> do
        insert (BlockTimeStrike { blockTimeStrikeBlock = height
                                , blockTimeStrikeStrikeMediantime =
                                  fromIntegral
                                  $ blockHeaderMediantime currentTip
                                  + fromIntegral ((fromNatural (height - blockHeaderHeight currentTip)) * 600) -- assume 10 minutes time slices for strike mediantime
                                , blockTimeStrikeCreationTime = utcTimeToPOSIXSeconds now
                                , blockTimeStrikeObservedResult = Nothing
                                , blockTimeStrikeObservedBlockMediantime = Nothing
                                , blockTimeStrikeObservedBlockHash = Nothing
                                })
      return nonExistentStrikeHeights
  when (List.length nonExistentBlockHeights > 0) $ do
    runLogging $ $(logInfo) ("ensureStrikeExistsAhead: created future strikes: " <> (tshow nonExistentBlockHeights))

