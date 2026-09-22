{-- | Websocket with live notifications about offers, contracts and new
 - blocks, see 'Data.OpEnergy.Offer.API.V1.LiveMessage'.
 -}
{-# LANGUAGE TemplateHaskell #-}
module OpEnergy.Offer.Server.V1.WebSocketService
  ( webSocketConnection
  , publishLiveEvent
  ) where

import           Control.Monad (forever, forM_, when)
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
import           Data.Word (Word64)

import qualified Network.WebSockets as WS

import qualified Data.OpEnergy.Account.API.V1.Account as AccountAPI
import qualified Data.OpEnergy.Account.API.V1.UUID as AccountAPI
import qualified Data.OpEnergy.Account.API.V2.WhoAmIResult as AccountV2
import           Data.OpEnergy.Offer.API.V1.LiveMessage
                 ( LiveRequest(..)
                 , LiveMessage(..)
                 , SequencedMessage(..)
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

-- | sends the given event to every websocket connection with the next
-- sequence number. The number is taken and the event is queued in one
-- transaction, so every connection receives the events in the order of
-- their numbers. Should be called only after the change has been committed
-- to the DB
publishLiveEvent :: MonadIO m => LiveEvent -> AppT m ()
publishLiveEvent event = do
  State{ liveEvents = liveEventsV, liveEventSeq = liveEventSeqV } <- ask
  liftIO $ STM.atomically $ do
    seqNo <- (+ 1) <$> TVar.readTVar liveEventSeqV
    TVar.writeTVar liveEventSeqV $! seqNo
    TChan.writeTChan liveEventsV (seqNo, event)

-- | This procedure is the mainloop of every websocket connection, which:
--
-- - sends 'LiveMessageHello' and the current chain tip, if it is known;
-- - handles requests from the frontend;
-- - forwards live events to the frontend.
-- It returns when the connection is closed.
webSocketConnection :: MonadIO m => WS.Connection -> AppT m ()
webSocketConnection conn = do
  state@State{ liveEvents = liveEventsV
             , liveEventSeq = liveEventSeqV
             , currentTip = currentTipV
             , currentTipMediantime = currentTipMediantimeV
             } <- ask
  liftIO $ do
    -- subscribes and reads the state it starts from in one transaction, so
    -- the connection receives every event published after that state. A
    -- chain tip, which is stored but not published yet, comes twice: in the
    -- state and as the next block.new, which the frontend can apply again
    (eventsV, lastSeqNo, mtip, mmediantime) <- STM.atomically $ (,,,)
      <$> TChan.dupTChan liveEventsV
      <*> TVar.readTVar liveEventSeqV
      <*> TVar.readTVar currentTipV
      <*> TVar.readTVar currentTipMediantimeV
    personV <- TVar.newTVarIO Nothing
    sendLock <- MVar.newMVar ()
    let send :: SequencedMessage -> IO ()
        send message = MVar.withMVar sendLock $ \_ ->
          WS.sendTextData conn (Aeson.encode message)
    runAppT state $ runLogging $ $(logDebug)
      "webSocketConnection: new connection"
    eresult <- E.tryAny $ WS.withPingThread conn keepAlivePingSecs (return ())
      $ do
        -- sent before any event is forwarded, so nothing can overtake them
        send (SequencedMessage (Just lastSeqNo) LiveMessageHello)
        forM_ mtip $ \tip ->
          send (SequencedMessage Nothing (LiveMessageBlockNew tip mmediantime))
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
  -> (SequencedMessage -> IO ())
  -> TVar (Maybe (AccountAPI.UUID AccountAPI.Person))
  -> IO ()
handleRequest state conn send personV = do
  (request :: BL.ByteString) <- WS.receiveData conn
  case Aeson.decode request of
    Nothing -> runAppT state $ runLogging $ $(logDebug)
      ( "handleRequest: ignoring unexpected request: " <> tshow request )
    Just LiveRequestPing -> send (SequencedMessage Nothing LiveMessagePong)
    -- the chain tip has already been sent when the connection opened
    Just LiveRequestInit -> return ()
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

-- | waits for the next live event and sends it to the frontend with its
-- sequence number, followed by 'LiveMessageMyChanged' if the event affects
-- the connection's account
forwardEvent
  :: (SequencedMessage -> IO ())
  -> TVar (Maybe (AccountAPI.UUID AccountAPI.Person))
  -> TChan (Word64, LiveEvent)
  -> IO ()
forwardEvent send personV eventsV = do
  ((seqNo, LiveEvent message persons), mperson) <- STM.atomically $ do
    event <- TChan.readTChan eventsV
    mperson <- TVar.readTVar personV
    return (event, mperson)
  send (SequencedMessage (Just seqNo) message)
  when (maybe False (`elem` persons) mperson) $
    send (SequencedMessage Nothing LiveMessageMyChanged)
