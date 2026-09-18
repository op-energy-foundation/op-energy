{-- | Offer data types: the response and request types for the Offer API.
 -}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DuplicateRecordFields      #-}
module Data.OpEnergy.Offer.API.V1.OfferInfo
  ( OfferInfo(..)
  , defaultOfferInfo
  , PaginatedOffers(..)
  , defaultPaginatedOffers
  , MyOffersResult(..)
  , defaultMyOffersResult
  , PostOfferRequest(..)
  , defaultPostOfferRequest
  , PostOfferResult(..)
  , defaultPostOfferResult
  ) where

import           Data.Swagger
import           Control.Lens
import           GHC.Generics
import           Data.Typeable              (Typeable)
import           Data.Aeson
import           Data.Time.Clock            (UTCTime)
import           Data.Word                  (Word64)

import           Data.OpEnergy.API.V1.Block (BlockHeight, defaultBlockHeight)
import           Data.OpEnergy.Account.API.V1.Account
                 ( DisplayName, defaultDisplayName
                 )
import           Data.OpEnergy.Offer.API.V1.OfferStatus
                 ( OfferStatus, defaultOfferStatus
                 )
import           Data.OpEnergy.Offer.API.V1.OfferSide
                 ( OfferSide, defaultOfferSide
                 )
import           Data.OpEnergy.Offer.API.V1.OfferID
                 ( OfferID, defaultOfferID
                 )
import           Data.OpEnergy.Offer.API.V1.ContractInfo
                 ( ContractInfo, defaultContractInfo
                 )

-- | one offer group, as returned by post/mine/list/:id/cancel
data OfferInfo = OfferInfo
  { offerId            :: OfferID
  , creatorDisplayName :: DisplayName
  , targetBlock        :: BlockHeight
  , mtpCutoffEpoch     :: Word64
  , side               :: OfferSide
  , validTillBlock     :: BlockHeight
  , makerStakeSats     :: Word64
  , takerStakeSats     :: Word64
  , blockRate          :: Double
  , totalContracts     :: Word64
  , matchedCount       :: Word64
  , createdAtBlock     :: BlockHeight
  , status             :: OfferStatus
  , expiresAt          :: Maybe UTCTime
  , refundedAt         :: Maybe UTCTime
  , created            :: UTCTime
  }
  deriving (Show, Generic, Typeable)
instance ToJSON   OfferInfo
instance FromJSON OfferInfo
instance ToSchema OfferInfo where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "OfferInfo schema"
    & mapped.schema.example ?~ toJSON defaultOfferInfo

defaultOfferInfo :: OfferInfo
defaultOfferInfo = OfferInfo
  { offerId = defaultOfferID
  , creatorDisplayName = defaultDisplayName
  , targetBlock = defaultBlockHeight
  , mtpCutoffEpoch = 0
  , side = defaultOfferSide
  , validTillBlock = defaultBlockHeight
  , makerStakeSats = 50000
  , takerStakeSats = 50000
  , blockRate = 10.0
  , totalContracts = 1
  , matchedCount = 0
  , createdAtBlock = defaultBlockHeight
  , status = defaultOfferStatus
  , expiresAt = Nothing
  , refundedAt = Nothing
  , created = read "2026-08-14 12:00:00 UTC"
  }

-- | paginated listing of offers and their contracts
data PaginatedOffers = PaginatedOffers
  { offers     :: [OfferInfo]
  , contracts  :: [ContractInfo]
  , page       :: Word64
  , limit      :: Word64
  , totalCount :: Word64
  }
  deriving (Show, Generic, Typeable)
instance ToJSON   PaginatedOffers
instance FromJSON PaginatedOffers
instance ToSchema PaginatedOffers where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "PaginatedOffers schema"
    & mapped.schema.example ?~ toJSON defaultPaginatedOffers

defaultPaginatedOffers :: PaginatedOffers
defaultPaginatedOffers = PaginatedOffers
  { offers = [ defaultOfferInfo ]
  , contracts = [ defaultContractInfo ]
  , page = 1
  , limit = 20
  , totalCount = 1
  }

-- | response for the /mine endpoint — the user's offers and contracts
data MyOffersResult = MyOffersResult
  { offers    :: [OfferInfo]
  , contracts :: [ContractInfo]
  }
  deriving (Show, Generic, Typeable)
instance ToJSON   MyOffersResult
instance FromJSON MyOffersResult
instance ToSchema MyOffersResult where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "MyOffersResult schema"
    & mapped.schema.example ?~ toJSON defaultMyOffersResult

defaultMyOffersResult :: MyOffersResult
defaultMyOffersResult = MyOffersResult
  { offers = [ defaultOfferInfo ]
  , contracts = [ defaultContractInfo ]
  }

-- | request body for posting new offers
data PostOfferRequest = PostOfferRequest
  { targetBlock    :: BlockHeight
  , mtpCutoffEpoch :: Word64
  , side           :: OfferSide
  , validTillBlock :: BlockHeight
  , makerStakeSats :: Word64
  , blockRate      :: Double
  , totalContracts :: Word64
  , createdAtBlock :: BlockHeight
  }
  deriving (Show, Generic, Typeable)
instance ToJSON   PostOfferRequest
instance FromJSON PostOfferRequest
instance ToSchema PostOfferRequest where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "PostOfferRequest schema"
    & mapped.schema.example ?~ toJSON defaultPostOfferRequest

defaultPostOfferRequest :: PostOfferRequest
defaultPostOfferRequest = PostOfferRequest
  { targetBlock = defaultBlockHeight
  , mtpCutoffEpoch = 0
  , side = defaultOfferSide
  , validTillBlock = defaultBlockHeight
  , makerStakeSats = 50000
  , blockRate = 10.0
  , totalContracts = 1
  , createdAtBlock = defaultBlockHeight
  }

-- | result of posting offers
data PostOfferResult = PostOfferResult
  { offers :: [OfferInfo]
  }
  deriving (Show, Generic, Typeable)
instance ToJSON   PostOfferResult
instance FromJSON PostOfferResult
instance ToSchema PostOfferResult where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "PostOfferResult schema"
    & mapped.schema.example ?~ toJSON defaultPostOfferResult

defaultPostOfferResult :: PostOfferResult
defaultPostOfferResult = PostOfferResult
  { offers = [ defaultOfferInfo ]
  }
