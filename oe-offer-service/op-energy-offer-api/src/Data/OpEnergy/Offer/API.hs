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

import           Data.OpEnergy.API.Tags (Tags)

import           Data.OpEnergy.Offer.API.V1
import           Data.OpEnergy.Offer.API.V1.LiveMessage
                 ( LiveRequest
                 , SequencedMessage
                 )

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
-- swagger: 'OfferWebSocketDocAPI' is what describes it in @swagger.json@.
type OfferWebSocketAPI
  = "api" :> "v1" :> "offer" :> "ws" :> WebSocket

-- | Description of 'OfferWebSocketAPI' for @swagger.json@, which is served
-- by 'OfferWebSocketAPI' itself, never by this type: swagger knows no
-- websocket, so the messages are described as a request of the frontend
-- ('LiveRequest') and a message of the service ('SequencedMessage'), which
-- is the shape swagger can express.
type OfferWebSocketDocAPI
  = "api" :> "v1" :> "offer" :> "ws"
    :> Tags "Live notifications"
    :> Summary "websocket, not a POST: live notifications of offers, contracts, new blocks and the account's balance"
    :> Description
       "Open a websocket to /api/v1/offer/ws on this host, over ws or wss, \
       \and exchange the JSON messages below: the request body is a message \
       \that the frontend sends, the response is a message that the service \
       \sends. The service also sends messages on its own, whenever an offer \
       \or a contract changes, a block arrives or the authenticated \
       \account's balance changes, so it is not a request/response protocol \
       \-- it is described as one here because swagger has no websocket."
    :> ReqBody '[JSON] LiveRequest
    :> Post '[JSON] SequencedMessage

-- | Combined API of the Offer service with Swagger documentation.
type API
  = OfferSwaggerAPI
  :<|> OfferWebSocketAPI
  :<|> OfferAPI

-- | The API that @swagger.json@ describes: 'OfferAPI' and, as swagger
-- cannot describe a websocket on its own, the description of the websocket.
type DocumentedAPI
  = OfferAPI
  :<|> OfferWebSocketDocAPI

documentedAPI :: Proxy DocumentedAPI
documentedAPI = Proxy

-- | Swagger spec for the Offer API.
offerApiSwagger :: Swagger
offerApiSwagger = toSwagger documentedAPI
  & info.title   .~ "OpEnergy Offer API"
  & info.version .~ "1.0"
  & info.description ?~ "OpEnergy"
  & info.license ?~ ("MIT" & url ?~ URL "http://mit.com")

apiSwagger :: Swagger
apiSwagger = offerApiSwagger
