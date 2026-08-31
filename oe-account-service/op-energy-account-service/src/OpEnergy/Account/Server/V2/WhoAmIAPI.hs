{-# LANGUAGE GADTs                      #-}
module OpEnergy.Account.Server.V2.WhoAmIAPI
  ( handlers
  ) where

import           Servant

import           Data.OpEnergy.Account.API.V2.WhoAmIAPI
import           Data.OpEnergy.Account.API.V2.WhoAmIResult (WhoAmIResult)
import qualified Data.OpEnergy.Account.API.V1.Account as AccountV1
import           OpEnergy.Account.Server.V1.Class (AppM, AppT)

import qualified OpEnergy.Account.Server.V2.AccountService.Whoami
                 as Whoami

handlers :: ServerT WhoAmIAPI (AppT Handler)
handlers
  = ( Whoami.whoamiHandler
      :: AccountV1.AccountToken
      -> AppM WhoAmIResult
    )
