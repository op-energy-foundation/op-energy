{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DuplicateRecordFields      #-}
-- | Posting one or more maker offers. See docs/plans/post-offer-api.md for
-- scope. Deliberately its own Tag module, separate from
-- Data.OpEnergy.Offer.API.V2.OffersAPI: this is the one mutating,
-- stake-moving endpoint, everything else there is a read.
module Data.OpEnergy.Offer.API.V2.PostOfferAPI
  ( PostOfferAPI
  , PostOfferRequest(..)
  , defaultPostOfferRequest
  , PostOfferResult(..)
  , defaultPostOfferResult
  ) where

import           Data.Swagger hiding (Header)
import           Control.Lens
import           GHC.Generics
import           Data.Typeable              (Typeable)
import           Data.Aeson
import           Servant.API

import           Data.OpEnergy.API.V1.Block (BlockHeight, defaultBlockHeight)
import           Data.OpEnergy.Account.API.V1.Account (AccountToken)
import           Data.OpEnergy.Offer.API.V2.OffersAPI (OfferInfo, defaultOfferInfo)

type PostOfferAPI
  = Header'
    '[ Required
     , Strict
     , Description "Account token gotten from the account service's /login or /register"
     ]
     "Authorization"
     AccountToken -- require authentication
  :> ReqBody '[JSON] PostOfferRequest
  :> Description "Posts one or more maker offers (identical except their id), staking numberOfOffers*makerStakeSats sats out of the caller's sandbox wallet balance, atomically. There is no matching engine/settlement yet -- see docs/plans/post-offer-api.md."
  :> Post '[JSON] PostOfferResult

-- | POST /api/v2/offer/post request body
data PostOfferRequest = PostOfferRequest
  { targetBlock :: BlockHeight
  , validTillBlock :: BlockHeight
  , numberOfOffers :: Int
  , makerStakeSats :: Int
    -- ^ numberOfOffers/makerStakeSats bounds are re-validated server-side
    -- (OpEnergy.Offer.Server.V2.PostOfferAPI.Post) since they are plain
    -- Ints here, not their own bounded wire types.
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

-- | POST /api/v2/offer/post response -- the offer(s) just created, in full.
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
