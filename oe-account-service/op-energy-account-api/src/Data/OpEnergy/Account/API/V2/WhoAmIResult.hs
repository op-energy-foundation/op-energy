{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DuplicateRecordFields      #-}
module Data.OpEnergy.Account.API.V2.WhoAmIResult
  ( WhoAmIResult(..)
  , defaultWhoAmIResult
  ) where

import           Data.Swagger
import           Control.Lens
import           GHC.Generics
import           Data.Typeable              (Typeable)
import           Data.Aeson
import           Data.Word                  (Word64)

import           Data.OpEnergy.Account.API.V1.Account
                 ( Person, DisplayName, defaultDisplayName
                 )
import           Data.OpEnergy.Account.API.V1.UUID
                 ( UUID, defaultUUID
                 )

-- | resolves an account token to the account's stable cross-service
-- identity (personUUID), current display name, and current sandbox
-- wallet balance
data WhoAmIResult = WhoAmIResult
  { personUUID   :: UUID Person
  , displayName  :: DisplayName
  , balance      :: Word64
  }
  deriving (Show, Generic, Typeable)

defaultWhoAmIResult :: WhoAmIResult
defaultWhoAmIResult = WhoAmIResult defaultUUID defaultDisplayName 300000

instance ToJSON WhoAmIResult
instance FromJSON WhoAmIResult
instance ToSchema WhoAmIResult where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "WhoAmIResult schema"
    & mapped.schema.example ?~ toJSON defaultWhoAmIResult
