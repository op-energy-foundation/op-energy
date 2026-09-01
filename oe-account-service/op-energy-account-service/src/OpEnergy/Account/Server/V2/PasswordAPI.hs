{-- | Handler wiring for the Password API subset.
 -}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
module OpEnergy.Account.Server.V2.PasswordAPI
  ( handlers
  )where

import           Servant

import qualified Data.OpEnergy.Account.API.V1.Account as API
import           Data.OpEnergy.Account.API.V2.SetPasswordRequest
                 ( SetPasswordRequest
                 )
import           Data.OpEnergy.Account.API.V2.PasswordLoginRequest
                 ( PasswordLoginRequest
                 )
import           Data.OpEnergy.Account.API.V2.LoginResult
                 ( LoginResult
                 )
import           Data.OpEnergy.Account.API.V2.PasswordAPI
                 ( PasswordAPI
                 )
import           OpEnergy.Account.Server.V1.Class
                 ( AppM, AppT
                 )

import qualified OpEnergy.Account.Server.V2.AccountService.SetPassword
                 as SetPassword
import qualified OpEnergy.Account.Server.V2.AccountService.LoginByPassword
                 as LoginByPassword

-- | see Data.OpEnergy.Account.API.V2.PasswordAPI for the API definition
handlers :: ServerT PasswordAPI (AppT Handler)
handlers
  = ( SetPassword.setPasswordHandler
      :: API.AccountToken -> SetPasswordRequest -> AppM NoContent
    )

  :<|> ( LoginByPassword.loginByPasswordHandler
         :: PasswordLoginRequest -> AppM LoginResult
       )
