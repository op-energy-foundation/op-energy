{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
module Data.OpEnergy.Offer.API where

import           Control.Lens
import           Data.Proxy
import           Data.Swagger
import           Servant.API
import           Servant.Swagger

import           Data.OpEnergy.Offer.API.V1

offerAPI :: Proxy OfferAPI
offerAPI = Proxy

type OfferAPI
  = "api" :> "v1" :> "offer" :> OfferV1API

-- | API for serving @swagger.json@.
type OfferSwaggerAPI
  = "api" :> "v1" :> "offer" :> "swagger.json" :> Get '[JSON] Swagger

-- | Combined API of the Offer service with Swagger documentation.
type API
  = OfferSwaggerAPI
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
