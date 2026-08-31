{-# LANGUAGE GADTs                      #-}
module OpEnergy.Offer.Server.V2.OffersAPI
  ( handlers
  ) where

import           Servant

import           Data.OpEnergy.Offer.API.V2.OffersAPI
import           Data.OpEnergy.API.V1.Positive
import           Data.Text(Text)

import qualified Data.OpEnergy.Account.API.V1.Account as AccountV1
import           Data.OpEnergy.Offer.API.V1.OfferStatus(OfferStatus)
import           OpEnergy.Offer.Server.V1.Class (AppM, AppT)

import qualified OpEnergy.Offer.Server.V2.OffersAPI.GetMine as GetMine
import qualified OpEnergy.Offer.Server.V2.OffersAPI.GetList as GetList
import qualified OpEnergy.Offer.Server.V2.OffersAPI.GetById as GetById

handlers :: ServerT OffersAPI (AppT Handler)
handlers
  = ( GetMine.getMineHandler
      :: AccountV1.AccountToken
      -> AppM [OfferInfo]
    )

  :<|> ( GetList.getListHandler
         :: Maybe OfferStatus
         -> Maybe AccountV1.DisplayName
         -> Maybe (Positive Int)
         -> Maybe (Positive Int)
         -> AppM PaginatedOffers
       )

  :<|> ( GetById.getByIdHandler
         :: Text
         -> AppM OfferInfo
       )
