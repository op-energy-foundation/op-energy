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
--
-- Charset validation is delegated to 'DisplayName'\'s own 'FromJSON'
-- instance ('verifyDisplayName'). Note: there is currently no
-- server-side length limit beyond a 255-char truncation; the 3-20
-- character convention is enforced by the frontend only.
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
