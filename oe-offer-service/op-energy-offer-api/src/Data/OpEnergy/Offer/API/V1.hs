{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
-- | Top-level V1 Offer API.
module Data.OpEnergy.Offer.API.V1
  ( OfferV1API
  ) where

import           Servant.API

import           Data.OpEnergy.API.Tags

import qualified Data.OpEnergy.Offer.API.V1.OffersAPI as OffersAPI
import qualified Data.OpEnergy.Offer.API.V1.TelegramAPI as TelegramAPI

type OfferV1API
  = Tags "Offers API"
    :> OffersAPI.OffersAPI

  :<|> Tags "Trade Chat API"
    :> "telegram"
    :> TelegramAPI.TelegramAPI
