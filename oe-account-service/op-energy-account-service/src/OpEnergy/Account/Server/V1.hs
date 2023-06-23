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
module OpEnergy.Account.Server.V1
  ( schedulerIteration
  , accountServer
  , blockTimeServer
  )where

import           Servant
import           Control.Monad.IO.Class(MonadIO)
-- import           Control.Monad(forM)
-- import           Control.Monad.Reader(ask)
-- import           Control.Monad.Logger(logError)
-- import qualified Data.Text as T

import           Data.OpEnergy.Account.API.V1
-- import           Data.OpEnergy.Account.API.V1.Account
-- import           Data.OpEnergy.API.V1.Positive
-- import qualified OpEnergy.Account.Server.GitCommitHash as Server
-- import qualified OpEnergy.Account.Server.V1.Metrics as Metrics( MetricsState(..))
import           OpEnergy.Account.Server.V1.Class (AppT) -- , runLogging, State(..))
import           OpEnergy.Account.Server.V1.AccountService
-- import           OpEnergy.Account.Server.V1.WebSocketService(webSocketConnection)
-- import           Data.Text.Show(tshow)

import           Prometheus(MonadMonitor)
-- import qualified Prometheus as P


accountServer :: ServerT AccountV1API (AppT Handler)
accountServer
  = undefined
  :<|> OpEnergy.Account.Server.V1.AccountService.register
  :<|> OpEnergy.Account.Server.V1.AccountService.login
  :<|> undefined

blockTimeServer :: ServerT BlockTimeV1API (AppT Handler)
blockTimeServer = undefined

-- | one iteration that called from scheduler thread
schedulerIteration :: (MonadIO m, MonadMonitor m) => AppT m ()
schedulerIteration = return ()

-- returns just commit hash, provided by build system
-- oeGitHashGet :: AppT Handler GitHashResponse
-- oeGitHashGet = return $ GitHashResponse
--   { gitCommitHash = Server.gitCommitHash
--   }
