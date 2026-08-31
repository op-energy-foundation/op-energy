{-- |
 - This module is the top module of the Offer service's V2 API
 -}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module OpEnergy.Offer.Server.V2
  ( offerServer
  )where

import           Servant

import           OpEnergy.Offer.Server.V1.Class ( AppT)

import           Data.OpEnergy.Offer.API.V2 (OfferV2API)
import           Data.OpEnergy.Offer.API.V2.PostOfferAPI (PostOfferAPI)
import           Data.OpEnergy.Offer.API.V2.OffersAPI (OffersAPI)
import           Data.OpEnergy.Offer.API.V2.CancelAPI (CancelAPI)
import qualified OpEnergy.Offer.Server.V2.PostOfferAPI as PostOfferAPI
import qualified OpEnergy.Offer.Server.V2.OffersAPI as OffersAPI
import qualified OpEnergy.Offer.Server.V2.CancelAPI as CancelAPI

offerServer :: ServerT OfferV2API (AppT Handler)
offerServer
  =    (PostOfferAPI.handlers :: ServerT PostOfferAPI (AppT Handler))
  :<|> (OffersAPI.handlers    :: ServerT OffersAPI (AppT Handler))
  :<|> (CancelAPI.handlers    :: ServerT CancelAPI (AppT Handler))
