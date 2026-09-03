{-# LANGUAGE GADTs                      #-}
module OpEnergy.Offer.Server.V2.CancelAPI
  ( cancelHandler
  ) where

import           Data.OpEnergy.Offer.API.V1.OfferInfo
                 ( OfferID, OfferInfo
                 )
import qualified Data.OpEnergy.Account.API.V1.Account as AccountV1
import           OpEnergy.Offer.Server.V1.Class (AppM)

import qualified OpEnergy.Offer.Server.V2.CancelAPI.Cancel
                 as Cancel

cancelHandler
  :: OfferID
  -> AccountV1.AccountToken
  -> AppM OfferInfo
cancelHandler = Cancel.cancelHandler
