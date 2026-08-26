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
  :<|> "register"
    :> Description "Registers a new person and returns their freshly generated account secret and account token, along with the display name that was assigned to them. Unauthenticated, as this call is what mints the caller's first credentials. Same underlying registration as the V1 call of this name, but the result also carries the assigned display name, which the frontend shows immediately and would otherwise have to fetch separately."
    :> Post '[JSON] RegisterResultV2
  :<|> "me"
    :> Header'
       '[ Required
        , Strict
        , Description "Account token gotten from /login or /register"
        ]
        "Authorization"
        AccountToken -- require authentication
    :> Description "Returns the display name of the account identified by the given account token, and whether that account has a password set. Used to restore a session: the caller knows only its stored token and needs to learn who that token belongs to."
    :> Get '[JSON] AccountInfo
  :<|> "displayname"
    :> Header'
       '[ Required
        , Strict
        , Description "Account token gotten from /login or /register"
        ]
        "Authorization"
        AccountToken -- require authentication
    :> ReqBody '[JSON] DisplayName
    :> Description "Renames the account identified by the given account token. Fails if another person already uses the requested display name, as display names are unique."
    :> Post '[JSON] AccountInfo
  :<|> "displayname"
    :> "exists"
    :> Capture "displayName" DisplayName
    :> Description "Reports whether the given display name is already taken. Unauthenticated, and deliberately says nothing else about the account: it exists so a person choosing a name can be told it is unavailable before they submit."
    :> Get '[JSON] DisplayNameExistsResult

-- | result of the V2 'register' API call. Same as V1's RegisterResult plus
-- the assigned display name
data RegisterResultV2 = RegisterResultV2
  { accountSecret :: AccountSecret
  , accountToken :: AccountToken
  , personUUID :: UUID Person
  , displayName :: DisplayName
  }
  deriving (Show, Generic, Typeable)

defaultRegisterResultV2 :: RegisterResultV2
defaultRegisterResultV2 = RegisterResultV2
  defaultAccountSecret defaultAccountToken defaultUUID defaultDisplayName

instance ToJSON RegisterResultV2
instance FromJSON RegisterResultV2
instance ToSchema RegisterResultV2 where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "RegisterResultV2 schema"
    & mapped.schema.example ?~ toJSON defaultRegisterResultV2

-- | result of the 'me' and 'displayname' API calls
data AccountInfo = AccountInfo
  { displayName :: DisplayName
  , hasPassword :: Bool -- ^ whether this account can also be logged into with
                        -- a password, as well as with its account secret. A
                        -- client restoring a session knows only its stored
                        -- token, so it cannot tell this for itself
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


