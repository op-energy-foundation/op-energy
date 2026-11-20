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
import           Data.OpEnergy.Account.API.V2
import           Data.OpEnergy.BlockTime.API.V2

accountAPI :: Proxy AccountAPI
accountAPI = Proxy

accountPublicAPI :: Proxy AccountPublicAPI
accountPublicAPI = Proxy

blockTimeAPI :: Proxy BlockTimeAPI
blockTimeAPI = Proxy

accountBlockTimeAPI :: Proxy AccountBlockTimeAPI
accountBlockTimeAPI = Proxy

accountBlockTimePublicAPI :: Proxy AccountBlockTimePublicAPI
accountBlockTimePublicAPI = Proxy

type AccountAPI
  = "api" :> ( "v1" :> "account" :> AccountV1API {- V1 API -}
             :<|> "v2" :> "account" :> AccountV2API {- V2 API -}
             )

-- | the same API without its service-to-service endpoints, which the swagger
-- is generated from: publishing them would also publish the name of the
-- header carrying their shared secret
type AccountPublicAPI
  = "api" :> ( "v1" :> "account" :> AccountV1API {- V1 API -}
             :<|> "v2" :> "account" :> AccountV2PublicAPI {- V2 API -}
             )

type BlockTimeAPI
  = "api" :>
    ( "v1" :> "blocktime" :> BlockTimeV1API {- V1 API -}
    :<|> "v2" :> "strikes" :> "blockrate" :> BlockTimeV2API {- V2 API -}
    )

-- | Composition of Account and Blocktime APIs
type AccountBlockTimeAPI
  = AccountAPI :<|> BlockTimeAPI

-- | 'AccountBlockTimeAPI' without the service-to-service endpoints
type AccountBlockTimePublicAPI
  = AccountPublicAPI :<|> BlockTimeAPI

-- | API for serving @swagger.json@.
type AccountSwaggerAPI
  = "api" :> "v1" :> "account" :> "swagger.json" :> Get '[JSON] Swagger
type BlockTimeSwaggerAPI
  = "api" :> "v1" :> "blocktime" :> "swagger.json" :> Get '[JSON] Swagger

-- | Combined API of a Account, BlockTime services with Swagger documentation.
type API
  = AccountSwaggerAPI
  :<|> AccountAPI
  :<|> BlockTimeSwaggerAPI
  :<|> BlockTimeAPI

-- | Swagger spec for Todo API.
accountApiSwagger :: Swagger
accountApiSwagger = toSwagger accountPublicAPI
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

apiSwagger :: Swagger
apiSwagger = toSwagger accountBlockTimePublicAPI
  & info.title   .~ "OpEnergy Account and BlockTime API"
  & info.version .~ "2.0"
  & info.description ?~ "OpEnergy"
  & info.license ?~ ("MIT" & url ?~ URL "http://mit.com")

