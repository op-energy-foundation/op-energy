{-- | Handler wiring for the Admin API subset: what a moderator may do to
 - another account.
 -}
{-# LANGUAGE GADTs                      #-}
module OpEnergy.Account.Server.V2.AdminAPI
  ( handlers
  ) where

import           Servant

import           Data.OpEnergy.Account.API.V2.AdminAPI
import qualified Data.OpEnergy.Account.API.V1.Account
                 as API
import           Data.OpEnergy.Account.API.V1.UUID
                 ( UUID
                 )
import           Data.OpEnergy.Account.API.V2.BanUserResult
                 ( BanUserResult
                 )
import           OpEnergy.Account.Server.V1.Class
                 ( AppM, AppT
                 )

import qualified OpEnergy.Account.Server.V2.AdminAPI.BanUser
                 as BanUser

-- | see Data.OpEnergy.Account.API.V2.AdminAPI for the API definition
handlers :: ServerT AdminAPI (AppT Handler)
handlers
  = ( BanUser.banUserHandler
      :: UUID API.Person -> API.AccountToken -> AppM BanUserResult
    )
