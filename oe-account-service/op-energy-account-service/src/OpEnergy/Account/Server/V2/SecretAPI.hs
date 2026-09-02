{-- | Handler wiring for the Secret API subset (GET secret, POST
 - secret/regenerate).
 -}
{-# LANGUAGE GADTs                      #-}
module OpEnergy.Account.Server.V2.SecretAPI
  ( handlers
  ) where

import           Servant

import           Data.OpEnergy.Account.API.V2.SecretAPI
import qualified Data.OpEnergy.Account.API.V1.Account
                 as API
import           Data.OpEnergy.Account.API.V2.AccountSecretResult
                 ( AccountSecretResult
                 )
import           OpEnergy.Account.Server.V1.Class
                 ( AppM, AppT
                 )

import qualified OpEnergy.Account.Server.V2.AccountService.GetSecret
                 as GetSecret
import qualified OpEnergy.Account.Server.V2.AccountService.RegenerateSecret
                 as RegenerateSecret

-- | see Data.OpEnergy.Account.API.V2.SecretAPI for the API definition
handlers :: ServerT SecretAPI (AppT Handler)
handlers
  = ( GetSecret.getSecretHandler
      :: API.AccountToken -> AppM AccountSecretResult
    )

  :<|> ( RegenerateSecret.regenerateSecretHandler
         :: API.AccountToken -> AppM AccountSecretResult
       )
