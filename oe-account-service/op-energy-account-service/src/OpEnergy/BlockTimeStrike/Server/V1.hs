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
  , OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeService.newTipHandlerLoop
  )where

import           Servant

import           Control.Monad.IO.Class(MonadIO)
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TVar as TVar
import qualified Control.Concurrent.MVar as MVar
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
import qualified Data.OpEnergy.Account.API.V1.BlockTimeStrike as API
import qualified Data.OpEnergy.Account.API.V1.BlockTimeStrikeGuess as API
import qualified Data.OpEnergy.Account.API.V1.SlowFast as API
import qualified Data.OpEnergy.Account.API.V1.FilterRequest as API
import qualified Data.OpEnergy.Account.API.V1.PagingResult as API
import qualified Data.OpEnergy.Account.API.V1.UUID as API
import qualified Data.OpEnergy.Account.API.V1.Account as API
import           Data.OpEnergy.API.V1.Block
import           Data.OpEnergy.API.V1.Natural(Natural)
import           Data.OpEnergy.API.V1(GitHashResponse(..))
import           Data.OpEnergy.API.V1.WebSocketService.Message
import qualified OpEnergy.Account.Server.GitCommitHash as Server
import           OpEnergy.Account.Server.V1.Class (AppM, AppT, State(..), runAppT, runLogging)
import           OpEnergy.Account.Server.V1.Config
import           OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeService
import           OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeGuessService
import qualified OpEnergy.BlockTimeStrike.Server.V1.Class as BlockTime(State(..))

blockTimeServer :: ServerT BlockTimeV1API (AppT Handler)
blockTimeServer = websocketHandler
  :<|> ((createBlockTimeStrikeFutureHandler
        ) :: API.AccountToken
          -> BlockHeight
          -> Natural Int
          -> AppM ()
       )
  :<|>((OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeGuessService.createBlockTimeStrikeFutureGuess
       ) :: API.AccountToken
         -> BlockHeight
         -> Natural Int
         -> API.SlowFast
         -> AppM API.BlockTimeStrikeGuess
      )
  :<|> ((OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeService.getBlockTimeStrikesPage
        ) :: Maybe (Natural Int)
          -> Maybe (API.FilterRequest API.BlockTimeStrike API.BlockTimeStrikeFilter)
          -> AppM (API.PagingResult API.BlockTimeStrikeWithGuessesCount)
       )
  :<|> ((OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeGuessService.getBlockTimeStrikesGuessesPageHandler
        ) :: Maybe (Natural Int)
          -> Maybe ( API.FilterRequest
                     API.BlockTimeStrikeGuess
                     API.BlockTimeStrikeGuessFilter
                   )
          -> AppM (API.PagingResult API.BlockTimeStrikeGuess)
       )
  :<|> ((OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeGuessService.getBlockTimeStrikeGuessesPage
        ) :: BlockHeight
          -> Natural Int
          -> Maybe (Natural Int)
          -> Maybe ( API.FilterRequest
                     API.BlockTimeStrikeGuess
                     API.BlockTimeStrikeGuessFilter
                   )
          -> AppM (API.PagingResult API.BlockTimeStrikeGuess)
       )

  :<|> ((OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeService.getBlockTimeStrike
        ) :: BlockHeight
          -> Natural Int
          -> AppM API.BlockTimeStrikeWithGuessesCount
       )
  :<|> ((OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeGuessService.getBlockTimeStrikeGuess
        ) :: API.AccountToken
          -> BlockHeight
          -> Natural Int
          -> AppM API.BlockTimeStrikeGuess
       )
  :<|> ((OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeGuessService.getBlockTimeStrikeGuessPerson
        ) :: API.UUID API.Person
          -> BlockHeight
          -> Natural Int
          -> AppM API.BlockTimeStrikeGuess
       )
  :<|>  oeGitHashGet
  where
    websocketHandler :: MonadIO m => Connection-> AppT m ()
    websocketHandler = undefined

-- returns just commit hash, provided by build system
oeGitHashGet :: AppT Handler GitHashResponse
oeGitHashGet = return $ GitHashResponse
  { gitCommitHash = Server.gitCommitHash
  }

-- | one iteration that called from scheduler thread
schedulerIteration :: (MonadIO m, MonadMonitor m) => AppT m ()
schedulerIteration = return ()


-- | This function is the entry point for websocket client with which blocktime strike service will receive
-- notifications about discover new confirmed tip
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
      receiveConfirmedTipInLoop state conn
    -- | receive new confirmed tip notifications from backend
    receiveConfirmedTipInLoop :: State -> Connection-> IO ()
    receiveConfirmedTipInLoop state conn = do
      (tmsg :: Text) <- WS.receiveData conn
      let (mmsg :: Maybe Message) = Aeson.decode $ BS.fromStrict $ Text.encodeUtf8 tmsg
      case mmsg of
        Just msg -> handleMessage state msg
        Nothing -> return ()
      receiveConfirmedTipInLoop state conn
    -- | handle new message
    handleMessage _ MessagePong = return () -- ignore pong message
    handleMessage state (MessageNewestBlockHeader header unconfirmedBlockHeight) = -- update confirmed tip
      runAppT state $ do
        State{ blockTimeState =
               BlockTime.State
               { latestConfirmedBlock = latestConfirmedBlockV
               , blockTimeStrikeConfirmedTip = blockTimeStrikeConfirmedTipV
               , latestUnconfirmedBlockHeight = latestUnconfirmedBlockHeightV
               }
             } <- ask

        runLogging $ $(logInfo) $ "received new confirmed tip height: " <> (tshow $ blockHeaderHeight header)
        liftIO $ do
          STM.atomically $ do
            TVar.writeTVar latestConfirmedBlockV (Just header) -- blocktime and guesses create handlers will need this info
            TVar.writeTVar latestUnconfirmedBlockHeightV (Just unconfirmedBlockHeight)
          MVar.putMVar blockTimeStrikeConfirmedTipV header -- this way we will notify handler about new confirmed tip
