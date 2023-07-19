{-- |
 - This module is the top module of backend V1
 -}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DuplicateRecordFields      #-}
module OpEnergy.BlockTimeStrike.Server.V1
  ( blockTimeServer
  , schedulerIteration
  , runBlockSpanClient
  )where

import           Servant
import           Control.Monad.IO.Class(MonadIO)
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TVar as TVar
import           Control.Monad.Trans.Reader (ask)
import           Control.Monad.Logger (logInfo)
import           Control.Monad.IO.Class (liftIO)
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text
import           Data.Text.Show (tshow)
import qualified Data.ByteString.Lazy as BS

import           Network.WebSockets as WS( Connection, runClient, sendTextData, receiveData)
import           Servant.Client(BaseUrl(..))
import qualified Data.Aeson as Aeson
import           Prometheus(MonadMonitor)

import           Data.OpEnergy.Account.API.V1
import           Data.OpEnergy.Account.API.V1.Account
import           Data.OpEnergy.API.V1.Block
import           Data.OpEnergy.API.V1.Natural
import           Data.OpEnergy.API.V1(GitHashResponse(..))
import           Data.OpEnergy.Account.API.V1.BlockTimeStrike
import           Data.OpEnergy.Account.API.V1.BlockTimeStrikeGuess
import           Data.OpEnergy.API.V1.WebSocketService.Message
import qualified OpEnergy.Account.Server.GitCommitHash as Server
import           OpEnergy.Account.Server.V1.Class (AppT, State(..), runAppT, runLogging)
import           OpEnergy.Account.Server.V1.Config
import           OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeService
import qualified OpEnergy.BlockTimeStrike.Server.V1.Class as BlockTime(State(..))

blockTimeServer :: ServerT BlockTimeV1API (AppT Handler)
blockTimeServer = websocketHandler
  :<|> OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeService.getBlockTimeStrikeFuture
  :<|> createBlockTimeStrikeFuture
  :<|> getBlockTimeStrikeFutureGuesses
  :<|> createBlockTimeStrikeFutureGuess
  :<|> getBlockTimeStrikePast
  :<|> getBlockTimeStrikePastGuesses
  :<|> oeGitHashGet
  where
    websocketHandler :: MonadIO m => Connection-> AppT m ()
    websocketHandler = undefined
    getBlockTimeStrikeFutureGuesses :: MonadIO m => AccountToken-> BlockHeight-> Natural Int-> AppT m [ BlockTimeStrikeGuessPublic ]
    getBlockTimeStrikeFutureGuesses = undefined
    createBlockTimeStrikeFutureGuess :: MonadIO m => AccountToken-> BlockHeight-> Natural Int-> SlowFast-> AppT m ()
    createBlockTimeStrikeFutureGuess = undefined
    getBlockTimeStrikePastGuesses :: MonadIO m => AccountToken-> AppT m [BlockTimeStrikeGuessResultPublic]
    getBlockTimeStrikePastGuesses = undefined

-- returns just commit hash, provided by build system
oeGitHashGet :: AppT Handler GitHashResponse
oeGitHashGet = return $ GitHashResponse
  { gitCommitHash = Server.gitCommitHash
  }

-- | one iteration that called from scheduler thread
schedulerIteration :: (MonadIO m, MonadMonitor m) => AppT m ()
schedulerIteration = return ()


-- | This function is the entry point for websocket client with which blocktime strike service will receive
-- notifications about discover new current tip
runBlockSpanClient :: MonadIO m => AppT m ()
runBlockSpanClient = do
  state@State{ config = Config { configBlockTimeStrikeBlockSpanWebsocketAPIURL = burl}
             } <- ask
  liftIO $ do
    runAppT state $ do
        runLogging $ $(logInfo) $ "trying to connect to: " <> tshow burl
    WS.runClient (baseUrlHost burl) (baseUrlPort burl) (baseUrlPath burl) (clientMain state)
  where
    clientMain :: State-> Connection-> IO ()
    clientMain state conn = do
      WS.sendTextData conn $! ActionInit -- send initial message to backend
      receiveCurrentTipInLoop state conn
    -- | receive new current tip notifications from backend
    receiveCurrentTipInLoop :: State -> Connection-> IO ()
    receiveCurrentTipInLoop state conn = do
      (tmsg :: Text) <- WS.receiveData conn
      let (mmsg :: Maybe Message) = Aeson.decode $ BS.fromStrict $ Text.encodeUtf8 tmsg
      case mmsg of
        Just msg -> handleMessage state msg
        Nothing -> return ()
      receiveCurrentTipInLoop state conn
    -- | handle new message
    handleMessage _ MessagePong = return () -- ignore pong message
    handleMessage state (MessageNewestBlockHeader header) = -- update current tip
      runAppT state $ do
        State{ blockTimeState = BlockTime.State { currentTip = currentTipV } } <- ask

        runLogging $ $(logInfo) $ "received new current tip height: " <> (tshow $ blockHeaderHeight header)
        liftIO $ STM.atomically $ TVar.writeTVar currentTipV (Just header)
