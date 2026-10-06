{-- | Response of GET /api/v1/offer/:id/details
 -}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE OverloadedStrings          #-}
module Data.OpEnergy.Offer.API.V1.OfferDetails
  ( OfferDetails(..)
  , defaultOfferDetails
  ) where

import           Data.Swagger
import           Control.Lens
import           GHC.Generics
import           Data.Typeable              (Typeable)
import           Data.Aeson

import           Data.OpEnergy.Offer.API.V1.OfferInfo
                 ( OfferInfo, defaultOfferInfo
                 )
import           Data.OpEnergy.Offer.API.V1.ContractInfo
                 ( ContractInfo, defaultContractInfo
                 )

-- | one offer with all its contracts, whatever the offer's status
data OfferDetails = OfferDetails
  { offer     :: OfferInfo
  , contracts :: [ContractInfo]
    -- ^ oldest first, 'yourRole' is 'Nothing'
  }
  deriving (Show, Generic, Typeable)
instance ToJSON   OfferDetails
instance FromJSON OfferDetails
instance ToSchema OfferDetails where
  declareNamedSchema proxy =
    genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "OfferDetails schema"
    & mapped.schema.example ?~ toJSON defaultOfferDetails

defaultOfferDetails :: OfferDetails
defaultOfferDetails = OfferDetails
  { offer = defaultOfferInfo
  , contracts = [ defaultContractInfo ]
  }
