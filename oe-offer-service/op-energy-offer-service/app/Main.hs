{-- | This module is backend's entrypoint
 -}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleInstances          #-}
module Main where

import           Control.Concurrent.Async
import           Control.Monad.IO.Class( liftIO)
import           Control.Monad.Logger (runStdoutLoggingT, logInfo)

import           OpEnergy.Offer.Server
import           OpEnergy.Offer.Server.V1.Config
import           OpEnergy.Offer.Server.V1.Class (runAppT, runLogging)

-- | entry point
main :: IO ()
main = runStdoutLoggingT $ do
  config <- liftIO $ OpEnergy.Offer.Server.V1.Config.getConfigFromEnvironment
  (state, prometheusA) <- OpEnergy.Offer.Server.initState config
  runAppT state $ runLogging $ $(logInfo) "bootstrap tasks"
  OpEnergy.Offer.Server.bootstrapTasks state
  -- now spawn worker threads
  schedulerA <- liftIO $ asyncBound $ runAppT state $ do -- this is scheduler thread, which goal is to perform periodical tasks
    runLogging $ $(logInfo) "scheduler thread"
    OpEnergy.Offer.Server.schedulerMainLoop
  serverA <- liftIO $ asyncBound $ runAppT state $ do -- this thread is for serving HTTP requests
    runLogging $ $(logInfo) "serving API"
    runServer
  liftIO $ waitAnyCancel $ -- waits for any of threads to shutdown in order to shutdown the rest
    [ serverA
    , schedulerA
    , prometheusA
    ]
  return ()
