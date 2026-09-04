{-- | Offer data types: the response and request types for the Offer API.
 -}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DuplicateRecordFields      #-}
module Data.OpEnergy.Offer.API.V1.OfferInfo
  ( OfferID(..)
  , defaultOfferID
  , OfferInfo(..)
  , defaultOfferInfo
  , PaginatedOffers(..)
  , defaultPaginatedOffers
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
import           Data.Text                  (Text)
import           Data.Time.Clock            (UTCTime)
import           Data.Word                  (Word64)
import           Servant.API                (FromHttpApiData(..), ToHttpApiData(..))

import           Data.OpEnergy.API.V1.Block (BlockHeight, defaultBlockHeight)
import           Data.OpEnergy.Account.API.V1.Account
                 ( DisplayName, defaultDisplayName
                 )
import           Data.OpEnergy.Offer.API.V1.OfferStatus
                 ( OfferStatus, defaultOfferStatus
                 )

-- | typed wrapper for offer identifiers
newtype OfferID = OfferID { unOfferID :: Text }
  deriving (Show, Eq, Generic, Typeable)
instance ToJSON OfferID where
  toJSON (OfferID t) = toJSON t
instance FromJSON OfferID where
  parseJSON = withText "OfferID" $ pure . OfferID
instance ToSchema OfferID where
  declareNamedSchema _ = pure $ NamedSchema (Just "OfferID") $ mempty
    & type_ ?~ SwaggerString
    & example ?~ toJSON defaultOfferID
instance ToParamSchema OfferID where
  toParamSchema _ = mempty
    & type_ ?~ SwaggerString
instance FromHttpApiData OfferID where
  parseQueryParam = Right . OfferID
instance ToHttpApiData OfferID where
  toQueryParam (OfferID t) = t

defaultOfferID :: OfferID
defaultOfferID = OfferID "1"

-- | one offer, as returned by post/mine/list/:id/cancel
data OfferInfo = OfferInfo
  { offerId            :: OfferID
  , creatorDisplayName :: DisplayName
  , targetBlock        :: BlockHeight
  , validTillBlock     :: BlockHeight
  , makerStakeSats     :: Word64
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
  , validTillBlock = defaultBlockHeight
  , makerStakeSats = 50000
  , status = defaultOfferStatus
  , expiresAt = Nothing
  , refundedAt = Nothing
  , created = read "2026-08-14 12:00:00 UTC"
  }

-- | paginated listing of offers
data PaginatedOffers = PaginatedOffers
  { items      :: [OfferInfo]
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
  { items = [ defaultOfferInfo ]
  , page = 1
  , limit = 20
  , totalCount = 1
  }

-- | request body for posting new offers
data PostOfferRequest = PostOfferRequest
  { targetBlock    :: BlockHeight
  , validTillBlock :: BlockHeight
  , numberOfOffers :: Word64
  , makerStakeSats :: Word64
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
  , validTillBlock = defaultBlockHeight
  , numberOfOffers = 1
  , makerStakeSats = 50000
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
