{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
module Data.OpEnergy.Account.API where

import           Control.Lens
import           Data.Proxy
import           Data.Swagger
import           Servant.API
import           Servant.Swagger

import           Data.OpEnergy.Account.API.V1

accountAPI :: Proxy AccountAPI
accountAPI = Proxy

blockTimeAPI :: Proxy BlockTimeAPI
blockTimeAPI = Proxy

internalBlockTimeAPI :: Proxy InternalBlockTimeAPI
internalBlockTimeAPI = Proxy

accountBlockTimeAPI :: Proxy AccountBlockTimeAPI
accountBlockTimeAPI = Proxy

type AccountAPI
  = "api" :> "v1" :> "account" :> AccountV1API {- V1 API -}

type BlockTimeAPI
  = "api" :> "v1" :> "blocktime" :> BlockTimeV1API {- V1 API -}

type InternalBlockTimeAPI
  = "api" :> "v1" :> "blocktime" :> InternalBlockTimeV1API {- V1 API -}

-- | Composition of Account and Blocktime APIs
type AccountBlockTimeAPI
  = AccountAPI :<|> BlockTimeAPI

-- | API for serving @swagger.json@.
type AccountSwaggerAPI
  = "api" :> "v1" :> "account" :> "swagger.json" :> Get '[JSON] Swagger
type BlockTimeSwaggerAPI
  = "api" :> "v1" :> "blocktime" :> "swagger.json" :> Get '[JSON] Swagger

type InternalBlockTimeSwaggerAPI
  = "api" :> "v1" :> "blocktime" :> "swagger.json" :> Get '[JSON] Swagger

-- | Combined API of a Account, BlockTime services with Swagger documentation.
type API
  = AccountSwaggerAPI
  :<|> AccountAPI
  :<|> BlockTimeSwaggerAPI
  :<|> BlockTimeAPI
  :<|> "api" :> "v1" :> "blocktime" :> "internal" :> InternalBlockTimeSwaggerAPI -- serve swagger internal API with public API

-- | Combined internal API
type InternalSwaggerBlockTimeAPI
  =    InternalBlockTimeSwaggerAPI
  :<|> InternalBlockTimeAPI

-- | Swagger spec for Todo API.
accountApiSwagger :: Swagger
accountApiSwagger = toSwagger accountAPI
  & info.title   .~ "OpEnergy Account API"
  & info.version .~ "1.0"
  & info.description ?~ "OpEnergy"
  & info.license ?~ ("MIT" & url ?~ URL "http://mit.com")

blockTimeApiSwagger :: Swagger
blockTimeApiSwagger = toSwagger blockTimeAPI
  & info.title   .~ "OpEnergy BlockTime API"
  & info.version .~ "1.0"
  & info.description ?~ "OpEnergy"
  & info.license ?~ ("MIT" & url ?~ URL "http://mit.com")

internalBlockTimeApiSwagger :: Swagger
internalBlockTimeApiSwagger = toSwagger internalBlockTimeAPI
  & info.title   .~ "OpEnergy Internal BlockTime API"
  & info.version .~ "1.0"
  & info.description ?~ "OpEnergy"
  & info.license ?~ ("MIT" & url ?~ URL "http://mit.com")

apiSwagger :: Swagger
apiSwagger = toSwagger accountBlockTimeAPI
  & info.title   .~ "OpEnergy Account and BlockTime API"
  & info.version .~ "1.0"
  & info.description ?~ "OpEnergy"
  & info.license ?~ ("MIT" & url ?~ URL "http://mit.com")

