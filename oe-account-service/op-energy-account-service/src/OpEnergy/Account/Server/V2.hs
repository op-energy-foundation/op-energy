{-- |
 - This module is the top module of Account V2 API.
 - Each API subset is wired with an explicit ServerT annotation
 - referencing the sub-API type, so the typechecker can localize
 - errors to the specific subset rather than the whole AccountV2API.
 -}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
module OpEnergy.Account.Server.V2
  ( accountServer
  )where

import           Servant

import           Data.OpEnergy.Account.API.V2
                 ( AccountV2API
                 )
import           Data.OpEnergy.Account.API.V2.LoginAPI
                 ( LoginAPI
                 )
import           Data.OpEnergy.Account.API.V2.PasswordAPI
                 ( PasswordAPI
                 )
import           Data.OpEnergy.Account.API.V2.RegisterAPI
                 ( RegisterAPI
                 )
import           Data.OpEnergy.Account.API.V2.ProfileAPI
                 ( ProfileAPI
                 )
import           OpEnergy.Account.Server.V1.Class
                 ( AppT
                 )

import qualified OpEnergy.Account.Server.V2.AccountService.Login
                 as LoginHandlers
import qualified OpEnergy.Account.Server.V2.PasswordAPI
                 as PasswordAPIHandlers
import qualified OpEnergy.Account.Server.V2.RegisterAPI
                 as RegisterAPIHandlers
import qualified OpEnergy.Account.Server.V2.ProfileAPI
                 as ProfileAPIHandlers

-- | V2 account server wiring
accountServer :: ServerT AccountV2API (AppT Handler)
accountServer
  = ( LoginHandlers.loginHandler
      :: ServerT LoginAPI (AppT Handler)
    )

  :<|> ( PasswordAPIHandlers.handlers
         :: ServerT PasswordAPI (AppT Handler)
       )

  :<|> ( RegisterAPIHandlers.handlers
         :: ServerT RegisterAPI (AppT Handler)
       )

  :<|> ( ProfileAPIHandlers.handlers
         :: ServerT ProfileAPI (AppT Handler)
       )
