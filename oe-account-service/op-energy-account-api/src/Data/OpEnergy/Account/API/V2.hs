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
  :<|> "secret"
    :> Header'
       '[ Required
        , Strict
        , Description "Account token gotten from /login or /register"
        ]
        "Authorization"
        AccountToken -- require authentication
    :> Description "Returns the account secret of the account identified by the given account token, so that the frontend can display the secret link back to its owner. Fails for an account registered before secrets were stored recoverably, whose secret exists only as a hash: such an account has to regenerate to obtain a displayable one."
    :> Get '[JSON] AccountSecretResult
  :<|> "secret"
    :> "regenerate"
    :> Header'
       '[ Required
        , Strict
        , Description "Account token gotten from /login or /register"
        ]
        "Authorization"
        AccountToken -- require authentication
    :> Description "Replaces the account secret of the account identified by the given account token with a freshly generated one, and returns it. The previous secret stops working immediately, which is the point of the call: it is how a person revokes a secret link they have shared or lost. The account token is left alone, so the caller's own session survives the rotation."
    :> Post '[JSON] AccountSecretResult

-- | result of the 'secret' and 'secret/regenerate' API calls
data AccountSecretResult = AccountSecretResult
  { accountSecret :: AccountSecret
  }
  deriving (Show, Generic, Typeable)

defaultAccountSecretResult :: AccountSecretResult
defaultAccountSecretResult = AccountSecretResult defaultAccountSecret

instance ToJSON AccountSecretResult
instance FromJSON AccountSecretResult
instance ToSchema AccountSecretResult where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "AccountSecretResult schema"
    & mapped.schema.example ?~ toJSON defaultAccountSecretResult

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


