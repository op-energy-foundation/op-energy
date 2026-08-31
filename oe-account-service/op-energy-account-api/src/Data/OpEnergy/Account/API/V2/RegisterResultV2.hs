{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DuplicateRecordFields      #-}
module Data.OpEnergy.Account.API.V2.RegisterResultV2
  ( RegisterResultV2(..)
  , defaultRegisterResultV2
  ) where

import           Data.Swagger
import           Control.Lens
import           GHC.Generics
import           Data.Typeable              (Typeable)
import           Data.Aeson

import           Data.OpEnergy.Account.API.V1.Account
                 ( AccountSecret
                 , AccountToken
                 , DisplayName
                 , Person
                 , defaultAccountSecret
                 , defaultAccountToken
                 , defaultDisplayName
                 )
import           Data.OpEnergy.Account.API.V1.UUID
                 ( UUID
                 , defaultUUID
                 )

-- | result of the V2 'register' API call. Same as V1's RegisterResult
-- plus the assigned display name
data RegisterResultV2 = RegisterResultV2
  { accountSecret :: AccountSecret
  , accountToken :: AccountToken
  , personUUID :: UUID Person
  , displayName :: DisplayName
  }
  deriving (Show, Generic, Typeable)

defaultRegisterResultV2 :: RegisterResultV2
defaultRegisterResultV2 = RegisterResultV2
  defaultAccountSecret defaultAccountToken defaultUUID defaultDisplayName

instance ToJSON RegisterResultV2
instance FromJSON RegisterResultV2
instance ToSchema RegisterResultV2 where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "RegisterResultV2 schema"
    & mapped.schema.example ?~ toJSON defaultRegisterResultV2
