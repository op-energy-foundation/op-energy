{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DuplicateRecordFields      #-}
-- | Browsing offers: its own ("mine"), everyone's ("list", filtered and
-- paginated), or a single one by id. See docs/plans/post-offer-api.md for
-- the overall feature's scope. Posting (mutating) lives in
-- Data.OpEnergy.Offer.API.V2.PostOfferAPI; cancelling in
-- Data.OpEnergy.Offer.API.V2.CancelAPI -- kept separate so each Tag module
-- covers one coherent slice, per this project's per-Tag-module convention
-- (see e.g. Data.OpEnergy.BlockTime.API.V2.StrikesAPI / GuessAPI).
module Data.OpEnergy.Offer.API.V2.OffersAPI
  ( OffersAPI
  , OfferInfo(..)
  , defaultOfferInfo
  , PaginatedOffers(..)
  , defaultPaginatedOffers
  ) where

import           Data.Swagger hiding (Header) -- Header: prefer Servant.API's route combinator
import           Control.Lens
import           GHC.Generics
import           Data.Typeable              (Typeable)
import           Data.Aeson
import           Data.Text                  (Text)
import           Data.Time.Clock            (UTCTime)
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
        , Description "Account token gotten from the account service's /login or /register"
        ]
       "Authorization"
       AccountToken -- require authentication
    :> Description "Lists offers posted by the account identified by the given account token, newest first, unfiltered/unpaginated."
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
    :> Description "Public, unauthenticated listing of offers across every account -- the general order-book browse, as opposed to /mine's caller-scoped list."
    :> Get '[JSON] PaginatedOffers

  :<|> Capture "id" Text
    :> Description "Full details for a single offer by id (its stringified DB key). Public, unauthenticated, same as /list."
    :> Get '[JSON] OfferInfo

-- | one offer, as returned by /post (Data.OpEnergy.Offer.API.V2.PostOfferAPI),
-- /mine, /list, /:id, and /:id/cancel (Data.OpEnergy.Offer.API.V2.CancelAPI)
data OfferInfo = OfferInfo
  { offerId :: Text
  , creatorDisplayName :: DisplayName
    -- ^ a snapshot of the creator's display name taken when the offer was
    -- posted, not a live lookup -- see OpEnergy.Offer.Server.V1.Offer's
    -- module header (service package) for why, and docs/plans/post-offer-api.md's
    -- "known gaps" section for the tradeoff this accepts.
  , targetBlock :: BlockHeight
  , validTillBlock :: BlockHeight
  , makerStakeSats :: Int
  , status :: OfferStatus
  , expiresAt :: Maybe UTCTime
  , refundedAt :: Maybe UTCTime
    -- ^ set alongside status the moment this offer's stake is refunded
    -- (expired/cancelled). Nothing while "open".
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

-- | GET /api/v2/offer/list response. No pre-existing pagination
-- convention/type is shared across this repo's services to reuse instead
-- (checked: op-energy-account-api has none) -- a plain, from-scratch
-- page+limit+totalCount shape.
data PaginatedOffers = PaginatedOffers
  { items :: [OfferInfo]
  , page :: Int
  , limit :: Int
  , totalCount :: Int
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
