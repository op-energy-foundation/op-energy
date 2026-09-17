{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
module Data.OpEnergy.Offer.API where

import           Control.Lens
import           Data.Proxy
import           Data.Swagger
import           Servant.API
import           Servant.API.WebSocket (WebSocket)
import           Servant.Swagger

import           Data.OpEnergy.Offer.API.V1

offerAPI :: Proxy OfferAPI
offerAPI = Proxy

type OfferAPI
  = "api" :> "v1" :> "offer" :> OfferV1API

-- | API for serving @swagger.json@.
type OfferSwaggerAPI
  = "api" :> "v1" :> "offer" :> "swagger.json" :> Get '[JSON] Swagger

-- | API for the websocket with live notifications, see
-- 'Data.OpEnergy.Offer.API.V1.LiveMessage'. It has to be separate from
-- 'OfferAPI', as websockets are supported neither by servant-client nor by
-- swagger.
type OfferWebSocketAPI
  = "api" :> "v1" :> "offer" :> "ws" :> WebSocket

-- | Combined API of the Offer service with Swagger documentation.
type API
  = OfferSwaggerAPI
  :<|> OfferWebSocketAPI
  :<|> OfferAPI

-- | Swagger spec for the Offer API.
offerApiSwagger :: Swagger
offerApiSwagger = toSwagger offerAPI
  & info.title   .~ "OpEnergy Offer API"
  & info.version .~ "1.0"
  & info.description ?~ "OpEnergy"
  & info.license ?~ ("MIT" & url ?~ URL "http://mit.com")

apiSwagger :: Swagger
apiSwagger = offerApiSwagger
