{-- |
 - this module's goal is to be entrypoint between all the backend versions.
 -}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleInstances          #-}
module OpEnergy.Offer.Server where

import           System.IO as IO
import           Servant ( Application, Proxy(..), ServerT, serve, hoistServer, (:<|>)(..))
import           Network.Wai.Handler.Warp(run)
import           Control.Monad (when)
import           Control.Monad.Trans.Reader (ask)
import           Control.Concurrent (threadDelay)
import           Control.Monad.IO.Class(liftIO, MonadIO)
import           Control.Monad.IO.Unlift(MonadUnliftIO)
import           Control.Monad.Logger (MonadLoggerIO, askLoggerIO, logDebug, logError, LoggingT, NoLoggingT, runLoggingT, filterLogger)
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.STM.TVar as TVar
import           Control.Concurrent.Async

import           Prometheus(MonadMonitor)

import           Data.OpEnergy.Offer.API
import           Data.OpEnergy.API.V1.Positive
import           Data.Text.Show(tshow)

import           OpEnergy.Error(describeError)
import           OpEnergy.Offer.Server.V1.Config
import           OpEnergy.Offer.Server.V1.Class (AppT, AppM, State(..), defaultState, runAppT, runLogging)
import           OpEnergy.Offer.Server.V1.DB
import           OpEnergy.Offer.Server.V1.Metrics
import           OpEnergy.Offer.Server.V2 (offerServer)
import qualified OpEnergy.Offer.Server.V2.Expiry as Expiry
import qualified OpEnergy.Offer.Server.V2.Settlement as Settlement

-- required by prometheus-client
instance MonadMonitor (LoggingT IO)
instance MonadMonitor (NoLoggingT IO)

-- | reads config from file and opens DB connection
initState
  :: ( MonadLoggerIO m
     , MonadUnliftIO m
     )
  => Config
  -> m (State, Async ())
initState config = do
  logFunc <- askLoggerIO
  let Config{ configLogLevelMin = logLevelMin} = config
      filterUnwantedLevels _source level = level >= logLevelMin
      runLogging' action = runLoggingT (filterLogger filterUnwantedLevels action) logFunc
  pool <- runLogging' $ OpEnergy.Offer.Server.V1.DB.getConnection config
  metricsV <- liftIO $ MVar.newEmptyMVar
  prometheusA <- liftIO $ asyncBound $ OpEnergy.Offer.Server.V1.Metrics.runMetricsServer config metricsV
  metrics <- liftIO $ MVar.readMVar metricsV
  state <- defaultState config metrics logFunc pool
  return (state, prometheusA)

-- | Runs HTTP server on a port defined in config in the State datatype
runServer :: (MonadIO m) => AppT m ()
runServer = do
  s <- ask
  let port = configHTTPAPIPort (config s)
  liftIO $ run port (app s)
  where
    app :: State-> Application
    app s = serve api $ hoistServer api (runAppT s) serverSwaggerBackend
      where
        api :: Proxy API
        api = Proxy
        serverSwaggerBackend :: ServerT API AppM
        serverSwaggerBackend
          = (return apiSwagger)
          :<|> offerServer

-- | tasks, that should be running during start
bootstrapTasks :: (MonadLoggerIO m, MonadMonitor m) => State -> m ()
bootstrapTasks s = runAppT s $ do
  return ()

-- | main loop of the scheduler
schedulerMainLoop :: (MonadIO m, MonadMonitor m) => AppT m ()
schedulerMainLoop = do
  State{ config = Config{ configSchedulerPollRateSecs = delaySecs }
       , currentUnconfirmedTip = currentUnconfirmedTipV
       } <- ask
  runLogging $ $(logDebug) "scheduler main loop"
  liftIO $ IO.hFlush stdout
  mTip <- liftIO $ TVar.readTVarIO currentUnconfirmedTipV
  case mTip of
    Nothing -> return ()
    Just tip -> do
      eExpiredCount <- Expiry.expireStaleOffers tip
      case eExpiredCount of
        -- the tick is over either way: the sweep is reported rather than
        -- retried here, as the next tick runs it again
        Left err -> runLogging $ $(logError)
          ( "schedulerMainLoop: the expiry sweep failed at tip " <> tshow tip
          <> ", the next tick runs it again: " <> describeError err
          )
        Right expiredCount
          | expiredCount > 0 -> runLogging $ $(logDebug)
            (tshow expiredCount <> " offer(s) expired at tip " <> tshow tip)
          | otherwise -> return ()
  liftIO $ threadDelay ((fromPositive delaySecs) * 1000000)
  schedulerMainLoop
