{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DuplicateRecordFields      #-}
module Data.OpEnergy.Offer.API.V2.OffersAPI
  ( OffersAPI
  , OfferInfo(..)
  , defaultOfferInfo
  , PaginatedOffers(..)
  , defaultPaginatedOffers
  ) where

import           Data.Swagger hiding (Header)
import           Control.Lens
import           GHC.Generics
import           Data.Typeable              (Typeable)
import           Data.Aeson
import           Data.Text                  (Text)
import           Data.Time.Clock            (UTCTime)
import           Data.Word                  (Word64)
import           Servant.API

import           Data.OpEnergy.API.V1.Block (BlockHeight, defaultBlockHeight)
import           Data.OpEnergy.API.V1.Positive
import           Data.OpEnergy.Account.API.V1.Account (AccountToken, DisplayName, defaultDisplayName)
import           Data.OpEnergy.Offer.API.V1.OfferStatus (OfferStatus, defaultOfferStatus)

type OffersAPI
  = "mine"
    :> Header'
       '[ Required
        , Strict
        , Description "Account token gotten from the account service's \
                      \/login or /register"
        ]
       "Authorization"
       AccountToken
    :> Description "Lists offers posted by the account identified by the \
                   \given account token, newest first."
    :> Get '[JSON] [OfferInfo]

  :<|> "list"
    :> QueryParam'
       '[ Optional
        , Strict
        , Description "restrict to offers currently in this status"
        ]
       "status"
       OfferStatus
    :> QueryParam'
       '[ Optional
        , Strict
        , Description "restrict to offers posted by this display name"
        ]
       "creatorDisplayName"
       DisplayName
    :> QueryParam'
       '[ Optional
        , Strict
        , Description "page number, starting at 1 (default 1)"
        ]
       "page"
       (Positive Int)
    :> QueryParam'
       '[ Optional
        , Strict
        , Description "results per page, default 20, capped at 100"
        ]
       "limit"
       (Positive Int)
    :> Description "Public, unauthenticated listing of offers across \
                   \every account."
    :> Get '[JSON] PaginatedOffers

  :<|> Capture "id" Text
    :> Description "Full details for a single offer by id."
    :> Get '[JSON] OfferInfo

-- | one offer, as returned by post/mine/list/:id/cancel
data OfferInfo = OfferInfo
  { offerId :: Text
  , creatorDisplayName :: DisplayName
  , targetBlock :: BlockHeight
  , validTillBlock :: BlockHeight
  , makerStakeSats :: Word64
  , status :: OfferStatus
  , expiresAt :: Maybe UTCTime
  , refundedAt :: Maybe UTCTime
  , created :: UTCTime
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
  { offerId = "1"
  , creatorDisplayName = defaultDisplayName
  , targetBlock = defaultBlockHeight
  , validTillBlock = defaultBlockHeight
  , makerStakeSats = 50000
  , status = defaultOfferStatus
  , expiresAt = Nothing
  , refundedAt = Nothing
  , created = read "2026-08-14 12:00:00 UTC"
  }

data PaginatedOffers = PaginatedOffers
  { items :: [OfferInfo]
  , page :: Word64
  , limit :: Word64
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
