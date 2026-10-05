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
                 , AccountV2MockAPI
                 , AccountV2PublicAPI
                 )
import           Data.OpEnergy.Account.API.V2.WalletAPI
                 ( WalletAPI
                 )
import           Data.OpEnergy.Account.API.V2.WalletMockAPI
                 ( WalletMockAPI
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
import           Data.OpEnergy.Account.API.V2.SecretAPI
                 ( SecretAPI
                 )
import           Data.OpEnergy.Account.API.V2.WhoAmIAPI (WhoAmIAPI)
import           Data.OpEnergy.Account.API.V2.InternalBalanceAPI (InternalBalanceAPI)
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
import qualified OpEnergy.Account.Server.V2.SecretAPI
                 as SecretAPIHandlers
import qualified OpEnergy.Account.Server.V2.WhoAmIAPI
                 as WhoAmIAPIHandlers
import qualified OpEnergy.Account.Server.V2.InternalBalanceAPI
                 as InternalBalanceAPIHandlers
import qualified OpEnergy.Account.Server.V2.WalletAPI
                 as WalletAPIHandlers
import qualified OpEnergy.Account.Server.V2.WalletMockAPI
                 as WalletMockAPIHandlers

-- | V2 account server wiring
accountServer :: ServerT AccountV2API (AppT Handler)
accountServer
  = accountPublicServer
  :<|> ( accountMockServer
         :: ServerT AccountV2MockAPI (AppT Handler)
       )
  :<|> ( InternalBalanceAPIHandlers.handlers
         :: ServerT InternalBalanceAPI (AppT Handler)
       )

-- | handlers of the mock wallet, which are served but not published in the
-- swagger. The service can only answer them while the mock wallet is in
-- use: a real wallet offers no way to mark an invoice paid
accountMockServer :: ServerT AccountV2MockAPI (AppT Handler)
accountMockServer
  = ( WalletMockAPIHandlers.handlers
      :: ServerT WalletMockAPI (AppT Handler)
    )

-- | handlers of the endpoints, which browser clients may call
accountPublicServer :: ServerT AccountV2PublicAPI (AppT Handler)
accountPublicServer
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

  :<|> ( SecretAPIHandlers.handlers
         :: ServerT SecretAPI (AppT Handler)
       )

  :<|> ( WhoAmIAPIHandlers.handlers
         :: ServerT WhoAmIAPI (AppT Handler)
       )

  :<|> ( WalletAPIHandlers.handlers
         :: ServerT WalletAPI (AppT Handler)
       )
