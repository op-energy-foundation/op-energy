{-- | Handler wiring for the Register API subset.
 -}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
module OpEnergy.Account.Server.V2.RegisterAPI
  ( handlers
  )where

import           Servant

import           Data.OpEnergy.Account.API.V2.RegisterResultV2
                 ( RegisterResultV2
                 )
import           Data.OpEnergy.Account.API.V2.RegisterAPI
                 ( RegisterAPI
                 )
import           OpEnergy.Account.Server.V1.Class
                 ( AppM, AppT
                 )

import qualified OpEnergy.Account.Server.V2.AccountService.Register
                 as Register

-- | see Data.OpEnergy.Account.API.V2.RegisterAPI for the API definition
handlers :: ServerT RegisterAPI (AppT Handler)
handlers
  = ( Register.registerHandler
      :: AppM RegisterResultV2
    )
