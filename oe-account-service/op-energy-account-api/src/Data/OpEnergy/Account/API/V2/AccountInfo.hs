{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DuplicateRecordFields      #-}
module Data.OpEnergy.Account.API.V2.AccountInfo
  ( AccountInfo(..)
  , defaultAccountInfo
  ) where

import           Data.Swagger
import           Control.Lens
import           GHC.Generics
import           Data.Typeable              (Typeable)
import           Data.Aeson

import           Data.OpEnergy.Account.API.V1.Account
                 ( DisplayName
                 , defaultDisplayName
                 )

-- | result of the 'me' and 'displayname' API calls
data AccountInfo = AccountInfo
  { displayName :: DisplayName
  , hasPassword :: Bool
  }
  deriving (Show, Generic, Typeable)

defaultAccountInfo :: AccountInfo
defaultAccountInfo = AccountInfo defaultDisplayName False

instance ToJSON AccountInfo
instance FromJSON AccountInfo
instance ToSchema AccountInfo where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "AccountInfo schema"
    & mapped.schema.example ?~ toJSON defaultAccountInfo
