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
import           Data.OpEnergy.Account.API.V2.WhoAmIAPI (WhoAmIAPI)
import           Data.OpEnergy.Account.API.V2.WalletAPI (WalletAPI)
import           Data.OpEnergy.Account.API.V2.WalletMockAPI (WalletMockAPI)
import           Data.OpEnergy.Account.API.V2.InternalBalanceAPI (InternalBalanceAPI)

-- | Account V2 API, which browser clients may call. Each endpoint subset is
-- organized as a separate Tag, imported from its own module.
type AccountV2PublicAPI
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

  :<|> Tags "WhoAmI API"
    :> "whoami"
    :> WhoAmIAPI

  :<|> Tags "Wallet API"
    :> "wallet"
    :> WalletAPI

-- | endpoint of the mock wallet, which is served but left out of the
-- swagger: it exists only until a lightning node does, and a published
-- endpoint for marking an invoice paid would be read as part of the API
type AccountV2MockAPI
  = Tags "Wallet Mock API"
    :> "wallet" :> "mock"
    :> WalletMockAPI

-- | service-to-service endpoints, which no browser client may call: nginx
-- refuses them and they are left out of the swagger, so neither the routes
-- nor the name of their shared secret header are published
type AccountV2InternalAPI
  = Tags "Internal Balance API"
    :> "internal" :> "balance"
    :> InternalBalanceAPI

-- | everything the service serves on /api/v2/account
type AccountV2API
  = AccountV2PublicAPI
  :<|> AccountV2MockAPI
  :<|> AccountV2InternalAPI
