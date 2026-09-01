{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DuplicateRecordFields      #-}
module Data.OpEnergy.Account.API.V2.PasswordLoginRequest
  ( PasswordLoginRequest(..)
  , defaultPasswordLoginRequest
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
import           Data.OpEnergy.Account.API.V1.Password
                 ( Password(..)
                 )

-- | body of the 'login/password' API call
data PasswordLoginRequest = PasswordLoginRequest
  { displayName :: DisplayName
  , password :: Password
  }
  deriving (Show, Generic, Typeable)

defaultPasswordLoginRequest :: PasswordLoginRequest
defaultPasswordLoginRequest =
  PasswordLoginRequest defaultDisplayName (Password "somepassword")

instance ToJSON PasswordLoginRequest
instance FromJSON PasswordLoginRequest
instance ToSchema PasswordLoginRequest where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "PasswordLoginRequest schema"
    & mapped.schema.example ?~ toJSON defaultPasswordLoginRequest
