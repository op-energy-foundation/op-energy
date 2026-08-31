{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DuplicateRecordFields      #-}
module Data.OpEnergy.Account.API.V2.BalanceAdjustRequest
  ( BalanceAdjustRequest(..)
  , defaultBalanceAdjustRequest
  ) where

import           Data.Swagger
import           Control.Lens
import           GHC.Generics
import           Data.Typeable              (Typeable)
import           Data.Aeson
import           Data.Word                  (Word64)

import           Data.OpEnergy.Account.API.V1.Account
                 ( Person
                 )
import           Data.OpEnergy.Account.API.V1.UUID
                 ( UUID, defaultUUID
                 )

-- | body of the internal/balance/deduct and internal/balance/credit API
-- calls -- see those routes for what each does with it
data BalanceAdjustRequest = BalanceAdjustRequest
  { personUUID  :: UUID Person
  , amountSats  :: Word64
    -- ^ always given as a positive magnitude -- deduct subtracts it
    -- (failing rather than going negative), credit adds it
  }
  deriving (Show, Generic, Typeable)

defaultBalanceAdjustRequest :: BalanceAdjustRequest
defaultBalanceAdjustRequest = BalanceAdjustRequest defaultUUID 50000

instance ToJSON BalanceAdjustRequest
instance FromJSON BalanceAdjustRequest
instance ToSchema BalanceAdjustRequest where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "BalanceAdjustRequest schema"
    & mapped.schema.example ?~ toJSON defaultBalanceAdjustRequest
