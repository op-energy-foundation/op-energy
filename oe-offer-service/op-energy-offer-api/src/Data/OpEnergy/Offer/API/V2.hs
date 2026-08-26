{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DuplicateRecordFields      #-}
-- | Top-level V2 Offer API: composes the per-Tag modules under
-- Data.OpEnergy.Offer.API.V2.*. New, so -- per this project's account
-- service precedent ("everything new belongs in V2") -- this lives
-- entirely in V2.
module Data.OpEnergy.Offer.API.V2
  ( OfferV2API
  , module Data.OpEnergy.Offer.API.V2.PostOfferAPI
  , module Data.OpEnergy.Offer.API.V2.OffersAPI
  , module Data.OpEnergy.Offer.API.V2.CancelAPI
  ) where

import           Servant.API

import           Data.OpEnergy.API.Tags

import           Data.OpEnergy.Offer.API.V2.PostOfferAPI
import           Data.OpEnergy.Offer.API.V2.OffersAPI
import           Data.OpEnergy.Offer.API.V2.CancelAPI

type OfferV2API
  = Tags "Post Offer API"
    :> "post"
    :> PostOfferAPI

  :<|> Tags "Offers API"
    :> OffersAPI

  :<|> Tags "Cancel Offer API"
    :> CancelAPI
