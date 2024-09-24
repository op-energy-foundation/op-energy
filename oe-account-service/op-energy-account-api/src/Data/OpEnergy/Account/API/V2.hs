{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DuplicateRecordFields      #-}
module Data.OpEnergy.Account.API.V2 where

import           Data.Swagger
import           Control.Lens
import           GHC.Generics
import           Data.Typeable              (Typeable)
import           Data.Aeson

import           Servant.API

import           Data.OpEnergy.Account.API.V1.Account
import           Data.OpEnergy.Account.API.V1.UUID

-- | API specifications of a backend service for Swagger
type AccountV2API
  = "login"
    :> ReqBody '[JSON] AccountSecret
    :> Description "Performs login with given account secret. Returns LoginResult(token and person UUID) value for being used with the rest API calls. See 'register' API call description for the reference of expected frontend's behavior related to secrets and tokens"
    :> Post '[JSON] LoginResult

data LoginResult = LoginResult
  { accountToken  :: AccountToken
  , personUUID :: UUID Person
  }
  deriving (Show, Generic, Typeable)

defaultLoginResult :: LoginResult
defaultLoginResult = LoginResult defaultAccountToken defaultUUID

instance ToJSON LoginResult
instance FromJSON LoginResult
instance ToSchema LoginResult where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "LoginResult schema"
    & mapped.schema.example ?~ toJSON defaultLoginResult


