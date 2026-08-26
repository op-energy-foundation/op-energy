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

import           Data.OpEnergy.Offer.API.V2
import qualified OpEnergy.Offer.Server.V2.PostOfferAPI as PostOfferAPI
import qualified OpEnergy.Offer.Server.V2.OffersAPI as OffersAPI
import qualified OpEnergy.Offer.Server.V2.CancelAPI as CancelAPI

-- | this is the implementation of Data.OpEnergy.Offer.API.V2.OfferV2API.
-- Check that type for the reference and API documentation
offerServer :: ServerT OfferV2API (AppT Handler)
offerServer
  =    (PostOfferAPI.handlers :: ServerT PostOfferAPI (AppT Handler))
  :<|> (OffersAPI.handlers    :: ServerT OffersAPI (AppT Handler))
  :<|> (CancelAPI.handlers    :: ServerT CancelAPI (AppT Handler))
