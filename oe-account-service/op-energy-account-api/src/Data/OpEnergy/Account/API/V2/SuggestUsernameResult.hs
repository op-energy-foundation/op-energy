{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}
module Data.OpEnergy.Account.API.V2.SuggestUsernameResult
  ( SuggestUsernameResult(..)
  , defaultSuggestUsernameResult
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

-- | result of the 'displayname/suggest' API call
data SuggestUsernameResult = SuggestUsernameResult
  { username :: DisplayName
  }
  deriving (Show, Generic, Typeable)

defaultSuggestUsernameResult :: SuggestUsernameResult
defaultSuggestUsernameResult = SuggestUsernameResult defaultDisplayName

instance ToJSON SuggestUsernameResult
instance FromJSON SuggestUsernameResult
instance ToSchema SuggestUsernameResult where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "SuggestUsernameResult schema"
    & mapped.schema.example ?~ toJSON defaultSuggestUsernameResult
