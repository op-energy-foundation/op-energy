{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}
module Data.OpEnergy.Account.API.V2.RegisterRequest
  ( RegisterRequest(..)
  , defaultRegisterRequest
  ) where

import           Data.Swagger
import           Control.Lens
import           GHC.Generics
import           Data.Typeable              (Typeable)
import           Data.Aeson

import           Data.OpEnergy.Account.API.V1.Account
                 ( DisplayName
                 )

-- | request body for the V2 'register' API call. The display name is
-- optional: if omitted, the backend generates a BIP39-style name.
data RegisterRequest = RegisterRequest
  { displayName :: Maybe DisplayName
  }
  deriving (Show, Generic, Typeable)

defaultRegisterRequest :: RegisterRequest
defaultRegisterRequest = RegisterRequest Nothing

instance ToJSON RegisterRequest
instance FromJSON RegisterRequest
instance ToSchema RegisterRequest where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "RegisterRequest schema"
    & mapped.schema.example ?~ toJSON defaultRegisterRequest
