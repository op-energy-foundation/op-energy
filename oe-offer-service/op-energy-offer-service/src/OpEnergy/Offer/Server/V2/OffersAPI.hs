{-# LANGUAGE GADTs                      #-}
module OpEnergy.Offer.Server.V2.OffersAPI
  ( getMineHandler
  , getListHandler
  , getByIdHandler
  ) where

import           Data.OpEnergy.Offer.API.V1.OfferInfo
                 ( OfferID, OfferInfo, PaginatedOffers(..)
                 )
import           Data.OpEnergy.API.V1.Positive

import qualified Data.OpEnergy.Account.API.V1.Account as AccountV1
import           Data.OpEnergy.Offer.API.V1.OfferStatus(OfferStatus)
import           OpEnergy.Offer.Server.V1.Class (AppM)

import qualified OpEnergy.Offer.Server.V2.OffersAPI.GetMine as GetMine
import qualified OpEnergy.Offer.Server.V2.OffersAPI.GetList as GetList
import qualified OpEnergy.Offer.Server.V2.OffersAPI.GetById as GetById

getMineHandler
  :: AccountV1.AccountToken
  -> AppM [OfferInfo]
getMineHandler = GetMine.getMineHandler

getListHandler
  :: Maybe OfferStatus
  -> Maybe AccountV1.DisplayName
  -> Maybe (Positive Int)
  -> Maybe (Positive Int)
  -> AppM PaginatedOffers
getListHandler = GetList.getListHandler

getByIdHandler
  :: OfferID
  -> AppM OfferInfo
getByIdHandler = GetById.getByIdHandler
