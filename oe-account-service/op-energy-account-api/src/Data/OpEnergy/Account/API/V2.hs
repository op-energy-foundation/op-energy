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
import           Data.OpEnergy.Account.API.V1.Password
import           Data.OpEnergy.Account.API.V1.UUID

-- | API specifications of a backend service for Swagger
type AccountV2API
  = "login"
    :> ReqBody '[JSON] AccountSecret
    :> Description "Performs login with given account secret. Returns LoginResult(token and person UUID) value for being used with the rest API calls. See 'register' API call description for the reference of expected frontend's behavior related to secrets and tokens"
    :> Post '[JSON] LoginResult
  :<|> "password"
    :> Header'
       '[ Required
        , Strict
        , Description "Account token gotten from /login or /register"
        ]
        "Authorization"
        AccountToken -- require authentication
    :> ReqBody '[JSON] SetPasswordRequest
    :> Description "Sets or replaces the password of the account identified by the given account token. A person has no password until this call succeeds, and can only log in with their account secret until then. Requires the account token rather than the current password, so that a person who reached this account via their secret link can still set one."
    :> Post '[JSON] NoContent
  :<|> "login"
    :> "password"
    :> ReqBody '[JSON] PasswordLoginRequest
    :> Description "Performs login with a display name and password, for a person returning on a device that does not have their account secret. Returns the same LoginResult as the secret-based 'login' call above, so the caller treats both identically. Fails if the person has not set a password."
    :> Post '[JSON] LoginResult

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


