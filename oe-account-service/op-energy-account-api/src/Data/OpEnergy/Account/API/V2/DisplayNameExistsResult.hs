{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}
module Data.OpEnergy.Account.API.V2.DisplayNameExistsResult
  ( DisplayNameExistsResult(..)
  , defaultDisplayNameExistsResult
  ) where

import           Data.Swagger
import           Control.Lens
import           GHC.Generics
import           Data.Typeable              (Typeable)
import           Data.Aeson

-- | result of the 'displayname/exists' API call
data DisplayNameExistsResult = DisplayNameExistsResult
  { exists :: Bool
  }
  deriving (Show, Generic, Typeable)

defaultDisplayNameExistsResult :: DisplayNameExistsResult
defaultDisplayNameExistsResult = DisplayNameExistsResult False

instance ToJSON DisplayNameExistsResult
instance FromJSON DisplayNameExistsResult
instance ToSchema DisplayNameExistsResult where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "DisplayNameExistsResult schema"
    & mapped.schema.example ?~ toJSON defaultDisplayNameExistsResult
