-- | This module's main purpose is to contain a scheduled routine to create future time strikes automatically.
{-# LANGUAGE TemplateHaskell          #-}
module OpEnergy.BlockTimeStrike.Server.V1.BlockTimeScheduledStrikeCreation
  ( maybeCreateStrikes
  ) where

import qualified Data.List as List
import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, getPOSIXTime)
import Control.Monad.Reader(ask, asks)
import Control.Monad.IO.Class(MonadIO, liftIO)
import           Control.Monad.Logger( logInfo, logDebug, logError)
import Control.Monad(forM_, when)
import qualified Control.Concurrent.STM.TVar as TVar

import           Database.Persist.Postgresql
import           Prometheus(MonadMonitor)
import           Data.OpEnergy.Client as Blockspan

import           Data.OpEnergy.API.V1.Block
import           Data.OpEnergy.Account.API.V1.BlockTimeStrike
import           Data.OpEnergy.API.V1.Positive
import           Data.OpEnergy.API.V1.Natural
import           OpEnergy.Account.Server.V1.Class as Account (profile, AppT, State(..), runLogging, withDBTransaction)
import           OpEnergy.BlockTimeStrike.Server.V1.Class as BlockTime
import           OpEnergy.Account.Server.V1.Config
import           Data.Text.Show

-- | This function is being called when latest confirmed block had been discovered. You should expect, that it can be called by recieving notification from the blockspan service either at discover or at a time of connection of block time service to blockspan service. So it is expected, that it will run at each restart of the blockspan service or at reconnection to blockspan service or at blocktime service restart.
maybeCreateStrikes
  :: ( MonadIO m
     , MonadMonitor m
     )
  => BlockHeader
  -> AppT m ()
maybeCreateStrikes confirmedTip = do
  ensureGuessableStrikeExistsAhead confirmedTip
  ensureNextEpochGuessableStrikeExists confirmedTip

-- | this function creates necessary guessable strikes if they haven't been created yet as it tries to be idempotent.
-- NOTE: currently, we use latest unconfirmed block height + Config.configBlockTimeStrikeGuessMinimumBlockAheadCurrentTip as the minimum block height for a future strike.
ensureGuessableStrikeExistsAhead
  :: ( MonadIO m
     , MonadMonitor m
     )
  => BlockHeader
  -> AppT m ()
ensureGuessableStrikeExistsAhead confirmedTip = profile "ensureGuessableStrikeExistsAhead" $ do
  Account.State{ config = Config
         { configBlockTimeStrikeGuessMinimumBlockAheadCurrentTip = configBlockTimeStrikeGuessMinimumBlockAheadCurrentTip
         , configBlockTimeStrikeShouldExistsAheadCurrentTip = configBlockTimeStrikeShouldExistsAheadCurrentTip
         }
       , blockTimeState = BlockTime.State
         { latestUnconfirmedBlockHeight = latestUnconfirmedBlockHeightV
         }
       } <- ask
  mlatestUnconfirmedBlockHeight <- liftIO $ TVar.readTVarIO latestUnconfirmedBlockHeightV
  case mlatestUnconfirmedBlockHeight of
    Nothing -> return () -- in case if no tip known yet
    Just latestUnconfirmedBlockHeight -> do
      -- respect threshold of allowed guess
      let minimumStrikeHeight = latestUnconfirmedBlockHeight + naturalFromPositive configBlockTimeStrikeGuessMinimumBlockAheadCurrentTip
          maximumStrikeHeight = minimumStrikeHeight + naturalFromPositive configBlockTimeStrikeShouldExistsAheadCurrentTip
      runLogging $ $(logDebug) ("ensureStrikeExistsAhead: confirmedTip: " <> tshow confirmedTip <>  ", min/max: " <> tshow [minimumStrikeHeight, maximumStrikeHeight])
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
                                      $ blockHeaderMediantime confirmedTip
                                      + fromIntegral ((fromNatural (height - blockHeaderHeight confirmedTip)) * 600) -- assume 10 minutes time slices for strike mediantime
                                    , blockTimeStrikeCreationTime = utcTimeToPOSIXSeconds now
                                    })
          return nonExistentStrikeHeights
      when (List.length nonExistentBlockHeights > 0) $ do
        runLogging $ $(logInfo) ("ensureStrikeExistsAhead: created future strikes: " <> (tshow nonExistentBlockHeights))


ensureNextEpochGuessableStrikeExists
  :: ( MonadIO m
     , MonadMonitor m
     )
  => BlockHeader
  -> AppT m ()
ensureNextEpochGuessableStrikeExists confirmedTip = profile "ensureNextEpochGuessableStrikeExists" $ do
  configBlockspanURL <- asks (configBlockspanURL . config)
  let
      epochBlocks = 2016
      currentEpochMultiplier = blockHeaderHeight confirmedTip `div` epochBlocks
      currentEpochBlockHeight = epochBlocks * currentEpochMultiplier
      nextEpochMultiplier = currentEpochMultiplier + 1
      nextEpochBlock = epochBlocks * nextEpochMultiplier
  currentEpochBlockMediantime <-
    liftIO $! Blockspan.withClient configBlockspanURL $
      blockHeaderMediantime <$> getBlockByHeight currentEpochBlockHeight
  let
      nextEpochBlockMediantime = currentEpochBlockMediantime
                               + (fromIntegral ( fromNatural epochBlocks * 600))
  mmNextEpochBlockStrikeCreated <- withDBTransaction "" $ do
    anyNextEpochBlockStrikeExist <- selectFirst
      [ BlockTimeStrikeBlock ==. nextEpochBlock
      , BlockTimeStrikeStrikeMediantime ==. fromIntegral nextEpochBlockMediantime
      ][]
    case anyNextEpochBlockStrikeExist of
      Just _ -> return Nothing
      Nothing -> do
        now <- liftIO $ getPOSIXTime
        fmap Just $! insert $! BlockTimeStrike
          { blockTimeStrikeBlock = nextEpochBlock
          , blockTimeStrikeStrikeMediantime = fromIntegral nextEpochBlockMediantime
          , blockTimeStrikeCreationTime = now
          }
  case mmNextEpochBlockStrikeCreated of
    Nothing ->
      runLogging $ $(logError) $ "ensureNextEpochGuessableStrikeExists: DB query failed"
    Just Nothing -> return ()
    Just (Just _) ->
      runLogging $ $(logInfo) $ "ensureNextEpochGuessableStrikeExists: created strike ("
        <> tshow nextEpochBlock <> " / "
        <> tshow nextEpochBlockMediantime <> ")"

