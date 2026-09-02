{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DuplicateRecordFields      #-}
module Data.OpEnergy.Account.API.V2.AccountSecretResult
  ( AccountSecretResult(..)
  , defaultAccountSecretResult
  ) where

import           Data.Swagger
import           Control.Lens
import           GHC.Generics
import           Data.Typeable              (Typeable)
import           Data.Aeson

import           Data.OpEnergy.Account.API.V1.Account
                 ( AccountSecret
                 , defaultAccountSecret
                 )

-- | result of the 'secret' and 'secret/regenerate' API calls
data AccountSecretResult = AccountSecretResult
  { accountSecret :: AccountSecret
  }
  deriving (Show, Generic, Typeable)

defaultAccountSecretResult :: AccountSecretResult
defaultAccountSecretResult = AccountSecretResult defaultAccountSecret

instance ToJSON AccountSecretResult
instance FromJSON AccountSecretResult
instance ToSchema AccountSecretResult where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "AccountSecretResult schema"
    & mapped.schema.example ?~ toJSON defaultAccountSecretResult
