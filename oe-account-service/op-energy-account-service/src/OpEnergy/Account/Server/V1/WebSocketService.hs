{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module OpEnergy.Account.Server.V1.WebSocketService where

import           Data.Aeson as Aeson
import           Data.Text(Text)
import           Data.Text.Show (tshow)
import           Control.Exception as E
import           Control.Monad ( forever)
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Control.Monad.Trans.Reader (ask)
import           Control.Monad.Logger (logDebug, logError)
import           Data.IORef
import           Network.WebSockets ( Connection, receiveData, withPingThread, sendTextData)

import           OpEnergy.Account.Server.V1.Class (runLogging, AppT, State(..), runAppT)
import           Data.OpEnergy.API.V1.Hash( Hash, generateRandomHash)
import           Data.OpEnergy.API.V1.Positive(naturalFromPositive)
import           OpEnergy.Account.Server.V1.WebSocketService.Message
import           OpEnergy.Account.Server.V1.Config



-- | This procedure is an mainloop for every websocket connection, which:
-- - handles requests from clients;
-- - sends notification about newest confirmed block
-- - sends keepalive packets
webSocketConnection :: MonadIO m => Connection-> AppT m ()
webSocketConnection conn = do
  state <- ask
  liftIO $ bracket (runAppT state $ initConnection state) (\uuid -> runAppT state $ closeConnection state uuid) $ \uuid-> do
    let State{ config = Config { configWebsocketKeepAliveSecs = configWebsocketKeepAliveSecs} } = state
    timeoutCounterV <- newIORef (naturalFromPositive configWebsocketKeepAliveSecs)
    liftIO $ withPingThread conn 1 (checkIteration state uuid timeoutCounterV) $ forever $ runAppT state $ do
      req <- liftIO $ receiveData conn
      case req of
        ActionWant topics -> sendTopics topics -- handle requested topics
        ActionPing -> do
          liftIO $ do
            sendTextData conn MessagePong
            writeIORef timeoutCounterV (naturalFromPositive configWebsocketKeepAliveSecs)
        ActionInit -> do
          runLogging $ $(logDebug) (tshow uuid <> " init data request")
          liftIO $ do
            sendTextData conn $ Aeson.encode ("init":: Text)
            writeIORef timeoutCounterV (naturalFromPositive configWebsocketKeepAliveSecs )
  where
    checkIteration state _ timeoutCounterV = do
      decreaseTimeoutOrSendPing state
      where
        decreaseTimeoutOrSendPing :: State-> IO ()
        decreaseTimeoutOrSendPing state = do
          let State{ config = Config { configWebsocketKeepAliveSecs = configWebsocketKeepAliveSecs} } = state
          counter <- readIORef timeoutCounterV
          if counter == 0
            then do
              writeIORef timeoutCounterV (naturalFromPositive configWebsocketKeepAliveSecs)
              sendTextData conn ("{\"pong\": true}" :: Text)
            else writeIORef timeoutCounterV (pred counter) -- decrease counter

    initConnection :: MonadIO m => State-> AppT m Hash
    initConnection _state = do
      uuid <- liftIO $ generateRandomHash
      runLogging $ $(logDebug) (tshow uuid <> ": new websocket connection")
      return uuid

    closeConnection _ uuid = do
      runLogging $ $(logDebug) (tshow uuid <> ": closed websocket connection")
      return ()

    sendTopics :: MonadIO m => [Text] -> AppT m ()
    sendTopics [] = return ()
    sendTopics ("generatedaccounttoken" : rest) = do -- for now send dummy secret/token
      liftIO $ sendTextData conn ("{\"generatedAccountSecret\" : \"test\", \"generatedAccountToken\": \"test\"}"::Text)
      sendTopics rest
    sendTopics ( topic : rest) = do
      runLogging $ $(logError) ("received unsupported ActionWant " <> topic)
      sendTopics rest
