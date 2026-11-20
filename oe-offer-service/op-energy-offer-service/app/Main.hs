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
import qualified OpEnergy.Offer.Server.V1.BlockspanClient as BlockspanClient
import qualified OpEnergy.Offer.Server.V1.TelegramClient as TelegramClient

-- | entry point
main :: IO ()
main = runStdoutLoggingT $ do
  config <- liftIO $ OpEnergy.Offer.Server.V1.Config.getConfigFromEnvironment
  (state, prometheusA) <- OpEnergy.Offer.Server.initState config
  runAppT state $ runLogging $ $(logInfo) "bootstrap tasks"
  OpEnergy.Offer.Server.bootstrapTasks state
  schedulerA <- liftIO $ asyncBound $ runAppT state $ do
    runLogging $ $(logInfo) "scheduler thread"
    OpEnergy.Offer.Server.schedulerMainLoop
  blockspanClientA <- liftIO $ asyncBound $ runAppT state $ do
    runLogging $ $(logInfo) "blockspan websocket client thread"
    BlockspanClient.runBlockspanTipClient
  -- started only where there is a group to follow. A deployment without a
  -- telegram group leaves this thread out rather than running one, which
  -- has nothing to follow: the process waits on any of its threads ending,
  -- so a thread, which returned because it had no work, would stop the
  -- service
  telegramAs <- case ( configTelegramBotToken config
                     , configTelegramChatId config
                     ) of
    (Just token, Just chatId) -> do
      telegramA <- liftIO $ asyncBound $ runAppT state $ do
        runLogging $ $(logInfo) "telegram mirror thread"
        TelegramClient.runTelegramMirror token chatId
      return [ telegramA ]
    _ -> do
      runAppT state $ runLogging $ $(logInfo)
        "telegram is not configured, so the trade chat is not mirrored"
      return []
  serverA <- liftIO $ asyncBound $ runAppT state $ do
    runLogging $ $(logInfo) "serving API"
    runServer
  liftIO $ waitAnyCancel $
    [ serverA
    , schedulerA
    , blockspanClientA
    , prometheusA
    ] ++ telegramAs
  return ()
