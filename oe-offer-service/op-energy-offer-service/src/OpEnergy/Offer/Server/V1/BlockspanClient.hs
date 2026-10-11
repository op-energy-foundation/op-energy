{-- | Follows the chain tip announced by the blockspan service's websocket.
 -}
{-# LANGUAGE TemplateHaskell #-}
module OpEnergy.Offer.Server.V1.BlockspanClient
  ( runBlockspanTipClient
  ) where

import           Control.Concurrent (threadDelay)
import           Control.Monad (forever, when)
import           Control.Monad.Trans.Reader (ask)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Logger (logDebug, logInfo, logWarn)
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TVar as TVar
import qualified Control.Exception.Safe as E
import           Control.Exception (evaluate)
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString.Lazy as BS
import qualified Data.Aeson as Aeson

import qualified Network.WebSockets as WS
import           Servant.Client (BaseUrl(..))

import           Data.OpEnergy.API.V1.WebSocketService.Message
                 ( Message(..)
                 , WebsocketRequest(..)
                 )
import           Data.Text.Show (tshow)

import           OpEnergy.Offer.Server.V1.Class
                 ( AppT
                 , State(..)
                 , runAppT
                 , runLogging
                 )
import           OpEnergy.Offer.Server.V1.Config (Config(..))

-- | delay between attempts to (re)connect to the blockspan websocket
reconnectDelayMicroseconds :: Int
reconnectDelayMicroseconds = 5 * 1000000

-- | Follows the chain tip announced by blockspan service's websocket and
-- stores it in 'currentTip'. Any failure of the connection is logged and the
-- connection is re-established after 'reconnectDelayMicroseconds', so this
-- function never returns and never throws a synchronous exception. Until the
-- first tip arrives, 'currentTip' stays 'Nothing'.
runBlockspanTipClient :: MonadIO m => AppT m ()
runBlockspanTipClient = do
  state@State{ config = Config{ configBlockspanWebsocketURL = burl } } <- ask
  liftIO $ forever $ do
    runAppT state $ runLogging $ $(logInfo)
      ( "runBlockspanTipClient: connecting to " <> tshow burl )
    eresult <- E.tryAny $ WS.runClient
      (baseUrlHost burl)
      (baseUrlPort burl)
      (baseUrlPath burl)
      (receiveTipInLoop state)
    runAppT state $ runLogging $ $(logWarn)
      ( "runBlockspanTipClient: connection to " <> tshow burl
      <> " is closed (" <> either tshow (const "closed by blockspan service") eresult
      <> "), reconnecting in "
      <> tshow (reconnectDelayMicroseconds `div` 1000000) <> " seconds"
      )
    threadDelay reconnectDelayMicroseconds

-- | requests the current state and then handles every message from
-- blockspan service until the connection fails
receiveTipInLoop :: State -> WS.Connection -> IO ()
receiveTipInLoop state conn = do
  WS.sendTextData conn $! ActionInit
  forever $ do
    (tmsg :: Text) <- WS.receiveData conn
    -- 'Message' parser calls 'error' on messages it does not know, so the
    -- decoding is forced here to keep such messages from closing the
    -- connection
    emsg <- E.tryAny $ evaluate
      ( Aeson.decode (BS.fromStrict (Text.encodeUtf8 tmsg)) :: Maybe Message )
    case emsg of
      Right (Just msg) -> runAppT state $ handleMessage msg
      _ -> runAppT state $ runLogging $ $(logDebug)
        ( "receiveTipInLoop: ignoring unexpected message: " <> tmsg )

-- | stores the chain tip from a message of blockspan service. Blockspan
-- service reports the newest confirmed block together with the chain tip
-- height, which is the newest confirmed block's height plus the amount of
-- blocks it waits for confirmation.
handleMessage :: MonadIO m => Message -> AppT m ()
handleMessage MessagePong = return ()
-- The tip height is forced before it is stored: its parser throws on invalid
-- values, and an unevaluated error must not end up in 'currentTip'.
handleMessage (MessageNewestBlockHeader _confirmedBlock !tipHeight _mTipBlock) = do
  State{ currentTip = currentTipV } <- ask
  previousTip <- liftIO $ STM.atomically $ TVar.swapTVar currentTipV (Just tipHeight)
  when (previousTip /= Just tipHeight) $ runLogging $ $(logInfo)
    ( "handleMessage: new chain tip " <> tshow tipHeight )
