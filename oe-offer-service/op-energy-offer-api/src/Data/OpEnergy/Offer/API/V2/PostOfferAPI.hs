{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DuplicateRecordFields      #-}
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
import           Data.Word                  (Word64)
import           Servant.API

import           Data.OpEnergy.API.V1.Block (BlockHeight, defaultBlockHeight)
import           Data.OpEnergy.Account.API.V1.Account (AccountToken)
import           Data.OpEnergy.Offer.API.V2.OffersAPI (OfferInfo, defaultOfferInfo)

type PostOfferAPI
  = Header'
    '[ Required
     , Strict
     , Description "Account token gotten from the account service's \
                   \/login or /register"
     ]
     "Authorization"
     AccountToken
  :> ReqBody '[JSON] PostOfferRequest
  :> Description "Posts one or more maker offers, staking \
                 \numberOfOffers*makerStakeSats sats out of the caller's \
                 \sandbox wallet balance, atomically."
  :> Post '[JSON] PostOfferResult

data PostOfferRequest = PostOfferRequest
  { targetBlock :: BlockHeight
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
