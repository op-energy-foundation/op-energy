{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DuplicateRecordFields      #-}
module Data.OpEnergy.Account.API.V2.LoginResult
  ( LoginResult(..)
  , defaultLoginResult
  ) where

import           Data.Swagger
import           Control.Lens
import           GHC.Generics
import           Data.Typeable              (Typeable)
import           Data.Aeson

import           Data.OpEnergy.Account.API.V1.Account
                 ( AccountToken
                 , DisplayName
                 , defaultAccountToken
                 , defaultDisplayName
                 , Person
                 )
import           Data.OpEnergy.Account.API.V1.UUID
                 ( UUID
                 , defaultUUID
                 )

-- | result of the V2 'login' API call. Includes display name and
-- password status so the frontend does not need a follow-up @\/me@ call
data LoginResult = LoginResult
  { accountToken  :: AccountToken
  , personUUID    :: UUID Person
  , displayName   :: DisplayName
  , hasPassword   :: Bool
  }
  deriving (Show, Generic, Typeable)

defaultLoginResult :: LoginResult
defaultLoginResult = LoginResult defaultAccountToken defaultUUID defaultDisplayName False

instance ToJSON LoginResult
instance FromJSON LoginResult
instance ToSchema LoginResult where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "LoginResult schema"
    & mapped.schema.example ?~ toJSON defaultLoginResult
