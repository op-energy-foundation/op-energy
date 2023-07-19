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
  )where

import           Servant
import           Control.Monad.IO.Class(MonadIO)

import           Data.OpEnergy.Account.API.V1
import           Data.OpEnergy.API.V1(GitHashResponse(..))
import qualified OpEnergy.Account.Server.GitCommitHash as Server
import           OpEnergy.Account.Server.V1.Class (AppT)
import           OpEnergy.Account.Server.V1.AccountService

import           Prometheus(MonadMonitor)

-- | this is the implementation of OpEnergy.Account.API.V1.AccountV1API. Check this type for the reference and API
-- documentation
accountServer :: ServerT AccountV1API (AppT Handler)
accountServer
  = OpEnergy.Account.Server.V1.AccountService.register
  :<|> OpEnergy.Account.Server.V1.AccountService.login
  :<|> OpEnergy.Account.Server.V1.AccountService.postDisplayName
  :<|> oeGitHashGet

-- | one iteration that called from scheduler thread
schedulerIteration :: (MonadIO m, MonadMonitor m) => AppT m ()
schedulerIteration = return ()

-- returns just commit hash, provided by build system
oeGitHashGet :: AppT Handler GitHashResponse
oeGitHashGet = return $ GitHashResponse
  { gitCommitHash = Server.gitCommitHash
  }
