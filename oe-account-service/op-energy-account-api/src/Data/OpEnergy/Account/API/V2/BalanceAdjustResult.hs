{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE OverloadedStrings          #-}
module Data.OpEnergy.Account.API.V2.BalanceAdjustResult
  ( BalanceAdjustResult(..)
  , defaultBalanceAdjustResult
  ) where

import           Data.Swagger
import           Control.Lens
import           GHC.Generics
import           Data.Typeable              (Typeable)
import           Data.Aeson

import           Data.OpEnergy.Account.API.V1.Sats
                 ( Sats(..)
                 )

-- | result of the internal/balance/deduct and internal/balance/credit
-- API calls: the account's balance after the adjustment, and which pocket
-- the adjustment moved
data BalanceAdjustResult = BalanceAdjustResult
  { balance :: Sats
  , freeSatsMoved :: Sats
    -- ^ how much of the amount was free sats. Deduct spends free sats
    -- first, so this is what a caller records against the stake it just
    -- paid for, to return it to the same pocket later
  }
  deriving (Show, Generic, Typeable)

defaultBalanceAdjustResult :: BalanceAdjustResult
defaultBalanceAdjustResult = BalanceAdjustResult (Sats 250000) (Sats 0)

instance ToJSON BalanceAdjustResult
instance FromJSON BalanceAdjustResult
instance ToSchema BalanceAdjustResult where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "BalanceAdjustResult schema"
    & mapped.schema.example ?~ toJSON defaultBalanceAdjustResult
