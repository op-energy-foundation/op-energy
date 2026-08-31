{-# LANGUAGE GADTs                      #-}
module OpEnergy.Offer.Server.V2.CancelAPI
  ( handlers
  ) where

import           Servant
import           Data.Text(Text)

import           Data.OpEnergy.Offer.API.V2.CancelAPI
import           Data.OpEnergy.Offer.API.V2.OffersAPI(OfferInfo)
import qualified Data.OpEnergy.Account.API.V1.Account as AccountV1
import           OpEnergy.Offer.Server.V1.Class (AppM, AppT)

import qualified OpEnergy.Offer.Server.V2.CancelAPI.Cancel
                 as Cancel

handlers :: ServerT CancelAPI (AppT Handler)
handlers
  = ( Cancel.cancelHandler
      :: Text
      -> AccountV1.AccountToken
      -> AppM OfferInfo
    )
