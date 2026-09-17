{-- | Websocket with live notifications about offers, contracts and new
 - blocks, see 'Data.OpEnergy.Offer.API.V1.LiveMessage'.
 -}
{-# LANGUAGE TemplateHaskell #-}
module OpEnergy.Offer.Server.V1.WebSocketService
  ( webSocketConnection
  , publishLiveEvent
  ) where

import           Control.Monad (forever, when)
import           Control.Monad.Trans.Reader (ask)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Logger (logDebug)
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TChan as TChan
import           Control.Concurrent.STM.TChan (TChan)
import qualified Control.Concurrent.STM.TVar as TVar
import           Control.Concurrent.STM.TVar (TVar)
import qualified Control.Exception.Safe as E
import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson as Aeson

import qualified Network.WebSockets as WS

import qualified Data.OpEnergy.Account.API.V1.Account as AccountAPI
import qualified Data.OpEnergy.Account.API.V1.UUID as AccountAPI
import qualified Data.OpEnergy.Account.API.V2.WhoAmIResult as AccountV2
import           Data.OpEnergy.Offer.API.V1.LiveMessage
                 ( LiveRequest(..)
                 , LiveMessage(..)
                 )
import           Data.Text.Show (tshow)

import           OpEnergy.Offer.Server.V1.Class
                 ( AppT
                 , State(..)
                 , runAppT
                 , runLogging
                 )
import           OpEnergy.Offer.Server.V1.LiveEvent (LiveEvent(..))
import qualified OpEnergy.Offer.Server.V1.AccountClient as AccountClient
import           OpEnergy.Error (describeError)

-- | interval of websocket ping frames, which keep idle connections open
-- through proxies
keepAlivePingSecs :: Int
keepAlivePingSecs = 30

-- | sends the given event to every websocket connection. Should be called
-- only after the change has been committed to the DB
publishLiveEvent :: MonadIO m => LiveEvent -> AppT m ()
publishLiveEvent event = do
  State{ liveEvents = liveEventsV } <- ask
  liftIO $ STM.atomically $ TChan.writeTChan liveEventsV event

-- | This procedure is the mainloop of every websocket connection, which:
-- - handles requests from the frontend;
-- - forwards live events to the frontend.
-- It returns when the connection is closed.
webSocketConnection :: MonadIO m => WS.Connection -> AppT m ()
webSocketConnection conn = do
  state@State{ liveEvents = liveEventsV } <- ask
  liftIO $ do
    -- subscribe first, so no event, published after this point, is missed
    eventsV <- STM.atomically $ TChan.dupTChan liveEventsV
    personV <- TVar.newTVarIO Nothing
    sendLock <- MVar.newMVar ()
    let send :: LiveMessage -> IO ()
        send message = MVar.withMVar sendLock $ \_ ->
          WS.sendTextData conn (Aeson.encode message)
    runAppT state $ runLogging $ $(logDebug) "webSocketConnection: new connection"
    eresult <- E.tryAny $ WS.withPingThread conn keepAlivePingSecs (return ()) $
      Async.race_
        (forever $ handleRequest state conn send personV)
        (forever $ forwardEvent send personV eventsV)
    runAppT state $ runLogging $ $(logDebug)
      ( "webSocketConnection: connection closed: "
      <> either tshow (const "") eresult
      )

-- | receives and handles one request from the frontend
handleRequest
  :: State
  -> WS.Connection
  -> (LiveMessage -> IO ())
  -> TVar (Maybe (AccountAPI.UUID AccountAPI.Person))
  -> IO ()
handleRequest state conn send personV = do
  (request :: BL.ByteString) <- WS.receiveData conn
  case Aeson.decode request of
    Nothing -> runAppT state $ runLogging $ $(logDebug)
      ( "handleRequest: ignoring unexpected request: " <> tshow request )
    Just LiveRequestPing -> send LiveMessagePong
    Just LiveRequestInit -> do
      -- lets the frontend know the chain tip right away
      let State{ currentTip = currentTipV } = state
      mtip <- TVar.readTVarIO currentTipV
      mapM_ (send . LiveMessageBlockNew) mtip
    Just (LiveRequestAuth token) -> authenticate state personV token

-- | remembers the account of the given token, so the connection receives
-- 'LiveMessageMyChanged' for this account. An invalid token is ignored
authenticate
  :: State
  -> TVar (Maybe (AccountAPI.UUID AccountAPI.Person))
  -> AccountAPI.AccountToken
  -> IO ()
authenticate state personV token = runAppT state $ do
  eperson <- AccountClient.verifyAccountToken token
  case eperson of
    Right (AccountV2.WhoAmIResult personUUIDV _displayName _balance) ->
      liftIO $ STM.atomically $ TVar.writeTVar personV (Just personUUIDV)
    Left err -> runLogging $ $(logDebug)
      ( "authenticate: ignoring invalid token: " <> describeError err )

-- | waits for the next live event and sends it to the frontend, followed
-- by 'LiveMessageMyChanged' if the event affects the connection's account
forwardEvent
  :: (LiveMessage -> IO ())
  -> TVar (Maybe (AccountAPI.UUID AccountAPI.Person))
  -> TChan LiveEvent
  -> IO ()
forwardEvent send personV eventsV = do
  (LiveEvent message persons, mperson) <- STM.atomically $ do
    event <- TChan.readTChan eventsV
    mperson <- TVar.readTVar personV
    return (event, mperson)
  send message
  when (maybe False (`elem` persons) mperson) $ send LiveMessageMyChanged
