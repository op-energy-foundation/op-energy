{-- | Handler wiring for the Profile API subset.
 -}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
module OpEnergy.Account.Server.V2.ProfileAPI
  ( handlers
  )where

import           Servant

import qualified Data.OpEnergy.Account.API.V1.Account as API
import           Data.OpEnergy.Account.API.V2.AccountInfo
                 ( AccountInfo
                 )
import           Data.OpEnergy.Account.API.V2.DisplayNameExistsResult
                 ( DisplayNameExistsResult
                 )
import           Data.OpEnergy.Account.API.V2.ProfileAPI
                 ( ProfileAPI
                 )
import           OpEnergy.Account.Server.V1.Class
                 ( AppM, AppT
                 )

import qualified OpEnergy.Account.Server.V2.AccountService.GetMe
                 as GetMe
import qualified OpEnergy.Account.Server.V2.AccountService.PostDisplayName
                 as PostDisplayName
import qualified OpEnergy.Account.Server.V2.AccountService.DisplayNameExists
                 as DisplayNameExists

-- | see Data.OpEnergy.Account.API.V2.ProfileAPI for the API definition
handlers :: ServerT ProfileAPI (AppT Handler)
handlers
  = ( GetMe.getMeHandler
      :: API.AccountToken -> AppM AccountInfo
    )

  :<|> ( PostDisplayName.postDisplayNameHandler
         :: API.AccountToken -> API.DisplayName -> AppM AccountInfo
       )

  :<|> ( DisplayNameExists.displayNameExistsHandler
         :: API.DisplayName -> AppM DisplayNameExistsResult
       )
