{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}
module Data.OpEnergy.Account.API.V2.SetPasswordRequest
  ( SetPasswordRequest(..)
  , defaultSetPasswordRequest
  ) where

import           Data.Swagger
import           Control.Lens
import           GHC.Generics
import           Data.Typeable              (Typeable)
import           Data.Aeson

import           Data.OpEnergy.Account.API.V1.Password
                 ( Password(..)
                 )

-- | body of the 'password' API call
data SetPasswordRequest = SetPasswordRequest
  { password :: Password
  }
  deriving (Show, Generic, Typeable)

defaultSetPasswordRequest :: SetPasswordRequest
defaultSetPasswordRequest = SetPasswordRequest (Password "somepassword")

instance ToJSON SetPasswordRequest
instance FromJSON SetPasswordRequest
instance ToSchema SetPasswordRequest where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "SetPasswordRequest schema"
    & mapped.schema.example ?~ toJSON defaultSetPasswordRequest
