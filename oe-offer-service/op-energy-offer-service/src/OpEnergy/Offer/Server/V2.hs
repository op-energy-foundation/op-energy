{-- |
 - This module is the top module of the Offer service's V1 API.
 - Server module paths remain at V2 for historical reasons; only the
 - API types moved to V1.
 -}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module OpEnergy.Offer.Server.V2
  ( offerServer
  )where

import           Servant

import           OpEnergy.Offer.Server.V1.Class ( AppT)

import           Data.OpEnergy.Offer.API.V1 (OfferV1API)
import qualified OpEnergy.Offer.Server.V2.PostOfferAPI as PostOfferAPI
import qualified OpEnergy.Offer.Server.V2.OffersAPI as OffersAPI
import qualified OpEnergy.Offer.Server.V2.CancelAPI as CancelAPI

-- | V1 offer server wiring. OffersAPI is one combined sub-API:
-- post :<|> cancel :<|> mine :<|> list :<|> getById
offerServer :: ServerT OfferV1API (AppT Handler)
offerServer
  =    PostOfferAPI.postHandler
  :<|> CancelAPI.cancelHandler
  :<|> OffersAPI.getMineHandler
  :<|> OffersAPI.getListHandler
  :<|> OffersAPI.getByIdHandler
