{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
module Data.OpEnergy.Account.API.V2.PasswordAPI
  ( PasswordAPI
  ) where

import           Servant.API

import           Data.OpEnergy.Account.API.V1.Account
                 ( AccountToken
                 )
import           Data.OpEnergy.Account.API.V2.SetPasswordRequest
                 ( SetPasswordRequest
                 )
import           Data.OpEnergy.Account.API.V2.PasswordLoginRequest
                 ( PasswordLoginRequest
                 )
import           Data.OpEnergy.Account.API.V2.LoginResult
                 ( LoginResult
                 )

-- | Password API subset: set password and login by password.
type PasswordAPI
  = "password"
    :> Header'
       '[ Required
        , Strict
        , Description "Account token gotten from /login or /register"
        ]
       "Authorization"
       AccountToken
    :> ReqBody '[JSON] SetPasswordRequest
    :> Description "Sets or replaces the password of the account \
                   \identified by the given account token."
    :> Post '[JSON] NoContent

  :<|> "login"
    :> "password"
    :> ReqBody '[JSON] PasswordLoginRequest
    :> Description "Performs login with a display name and password."
    :> Post '[JSON] LoginResult
