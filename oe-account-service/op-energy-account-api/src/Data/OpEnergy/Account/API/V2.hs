{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
module Data.OpEnergy.Account.API.V2 where

import           Servant.API

import           Data.OpEnergy.API.Tags

import qualified Data.OpEnergy.Account.API.V2.LoginAPI as LoginAPI
import qualified Data.OpEnergy.Account.API.V2.PasswordAPI as PasswordAPI
import qualified Data.OpEnergy.Account.API.V2.RegisterAPI as RegisterAPI
import qualified Data.OpEnergy.Account.API.V2.ProfileAPI as ProfileAPI
import qualified Data.OpEnergy.Account.API.V2.SecretAPI as SecretAPI

-- | Account V2 API. Each endpoint subset is organized as a separate
-- Tag, imported from its own module.
type AccountV2API
  = Tags "Login API"
    :> "login"
    :> LoginAPI.LoginAPI

  :<|> Tags "Password API"
    :> PasswordAPI.PasswordAPI

  :<|> Tags "Register API"
    :> "register"
    :> RegisterAPI.RegisterAPI

  :<|> Tags "Profile API"
    :> ProfileAPI.ProfileAPI

  :<|> Tags "Secret API"
    :> "secret"
    :> SecretAPI.SecretAPI
