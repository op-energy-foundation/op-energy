{-- |
 - this module's goal is to be entrypoint between all the backend versions. Currently, there is onty V1 version.
 -}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleInstances          #-}
module OpEnergy.BlockTimeStrike.Server
  ( bootstrapTasks
  , schedulerMainLoop
  , OpEnergy.BlockTimeStrike.Server.V1.runBlockSpanClient
  , OpEnergy.BlockTimeStrike.Server.V1.newTipHandlerLoop
  ) where

import           System.IO as IO
import           Control.Monad.Trans.Reader (ask)
import           Control.Concurrent (threadDelay)
import           Control.Monad.IO.Class(liftIO, MonadIO)
import           Control.Monad.Logger (MonadLoggerIO, logDebug )

import           Prometheus(MonadMonitor)

import           Data.OpEnergy.API.V1.Positive
import           OpEnergy.Account.Server.V1.Config
import           OpEnergy.Account.Server.V1.Class (AppT, State(..), runAppT, runLogging)
import           OpEnergy.BlockTimeStrike.Server.V1


-- | tasks, that should be running during start
bootstrapTasks :: (MonadLoggerIO m, MonadMonitor m) => State -> m ()
bootstrapTasks s = runAppT s $ do
  return ()


-- | main loop of the scheduler. Exception in this procedure will cause app to fail
schedulerMainLoop :: (MonadIO m, MonadMonitor m) => AppT m ()
schedulerMainLoop = do
  State{ config = Config{ configSchedulerPollRateSecs = delaySecs }} <- ask
  runLogging $ $(logDebug) "scheduler main loop"
  liftIO $ IO.hFlush stdout
  OpEnergy.BlockTimeStrike.Server.V1.schedulerIteration
  liftIO $ threadDelay ((fromPositive delaySecs) * 1000000)
  schedulerMainLoop
