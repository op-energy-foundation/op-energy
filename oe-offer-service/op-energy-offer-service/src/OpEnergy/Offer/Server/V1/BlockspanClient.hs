{-- | Everything this service needs from the blockspan service: the chain tip,
 - followed over the blockspan websocket, and the mediantime of a given block,
 - requested over HTTP.
 -}
{-# LANGUAGE TemplateHaskell #-}
module OpEnergy.Offer.Server.V1.BlockspanClient
  ( runBlockspanTipClient
  , getBlockMediantime
  ) where

import           Control.Concurrent (threadDelay)
import           Control.Monad (forever, guard, when)
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
import           Data.Word (Word64)
import qualified Data.Aeson as Aeson

import qualified Network.WebSockets as WS
import           Servant.Client (BaseUrl(..))

import           Data.OpEnergy.API.V1.Block (BlockHeight, BlockHeader(..))
import           Data.OpEnergy.API.V1.WebSocketService.Message
                 ( Message(..)
                 , WebsocketRequest(..)
                 )
import qualified Data.OpEnergy.Client as Blockspan
import           Data.OpEnergy.Offer.API.V1.LiveMessage (LiveMessage(..))
import           Data.Text.Show (tshow)

import           OpEnergy.Offer.Server.V1.Class
                 ( AppT
                 , State(..)
                 , runAppT
                 , runLogging
                 )
import           OpEnergy.Offer.Server.V1.Config (Config(..))
import           OpEnergy.Offer.Server.V1.LiveEvent (LiveEvent(..))
import           OpEnergy.Offer.Server.V1.WebSocketService (publishLiveEvent)
import           OpEnergy.Error (CallstackError, blockspanRequestFailed)

-- | delay between attempts to (re)connect to the blockspan websocket
reconnectDelayMicroseconds :: Int
reconnectDelayMicroseconds = 5 * 1000000

-- | Follows the chain tip announced by blockspan service's websocket and
-- stores it in 'currentUnconfirmedTip'. Any failure of the connection is
-- logged and the connection is re-established after
-- 'reconnectDelayMicroseconds', so this function never returns and never
-- throws a synchronous exception. 'currentUnconfirmedTip' is 'Nothing'
-- until the first tip arrives and again whenever the connection is closed,
-- as a tip, which is not followed anymore, only gets older while the chain
-- moves on.
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
    -- the tip of a closed connection is not followed anymore, so it is
    -- forgotten rather than kept while the chain moves on without it: an
    -- accept, an expiry or a settlement would otherwise go by a height,
    -- which is only getting older. The next connection asks for the tip
    -- again with its ActionInit
    STM.atomically $ do
      TVar.writeTVar (currentUnconfirmedTip state) Nothing
      TVar.writeTVar (currentTipMediantime state) Nothing
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

-- | stores the chain tip from a message of blockspan service and publishes
-- 'LiveMessageBlockNew' when its height or mediantime has changed. Blockspan
-- service reports the newest confirmed block together with the chain tip
-- height, which is the newest confirmed block's height plus the amount of
-- blocks it waits for confirmation, and the tip's header, if it has it.
handleMessage :: MonadIO m => Message -> AppT m ()
handleMessage MessagePong = return ()
-- The tip height is forced before it is stored: its parser throws on invalid
-- values, and an unevaluated error must not end up in 'currentTip'.
handleMessage
    (MessageNewestBlockHeader _confirmedBlock !tipHeight mTipBlock) = do
  State{ currentUnconfirmedTip = currentUnconfirmedTipV
       , currentTipMediantime = currentTipMediantimeV
       } <- ask
  let !mreported = tipMediantime tipHeight mTipBlock
  (previousUnconfirmedTip, previousMediantime, mmediantime) <- liftIO $ STM.atomically $ do
    previousUnconfirmedTip <- TVar.swapTVar currentUnconfirmedTipV (Just tipHeight)
    previousMediantime <- TVar.readTVar currentTipMediantimeV
    -- a message without the tip's header keeps the known mediantime of the
    -- same tip
    let mmediantime = case mreported of
          Nothing | previousUnconfirmedTip == Just tipHeight -> previousMediantime
          _ -> mreported
    TVar.writeTVar currentTipMediantimeV $! mmediantime
    return (previousUnconfirmedTip, previousMediantime, mmediantime)
  when ((previousUnconfirmedTip, previousMediantime) /= (Just tipHeight, mmediantime)) $ do
    runLogging $ $(logInfo)
      ( "handleMessage: publishing block.new: height " <> tshow tipHeight
      <> ", mediantime " <> maybe "unknown" tshow mmediantime
      )
    publishLiveEvent $! LiveEvent (LiveMessageBlockNew tipHeight mmediantime) []

-- | mediantime of the given tip header, if it is the header of the block at
-- the given height. Blockspan service keeps sending the previous tip's
-- header, when it fails to fetch the new one.
tipMediantime :: BlockHeight -> Maybe BlockHeader -> Maybe Word64
tipMediantime tipHeight mTipBlock = do
  tipBlock <- mTipBlock
  guard (blockHeaderHeight tipBlock == tipHeight)
  return $! fromIntegral (blockHeaderMediantime tipBlock)

-- | returns mediantime of the block with the given height, as reported by
-- blockspan service's HTTP API
getBlockMediantime
  :: MonadIO m
  => BlockHeight
  -> AppT m (Either CallstackError Word64)
getBlockMediantime height = do
  State{ config = Config{ configBlockspanURL = burl } } <- ask
  liftIO $! E.handle onException $ do
    eheader <- Blockspan.withClientEither burl (Blockspan.getBlockByHeight height)
    return $! either
      (Left . blockspanRequestFailed . tshow)
      (Right . fromIntegral . blockHeaderMediantime)
      eheader
  where
    onException :: E.SomeException -> IO (Either CallstackError Word64)
    onException err = return $! Left (blockspanRequestFailed (tshow err))
