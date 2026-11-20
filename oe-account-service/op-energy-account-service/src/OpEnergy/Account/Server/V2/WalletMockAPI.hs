{-- | Handler wiring for the mock wallet API subset.
 -}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
module OpEnergy.Account.Server.V2.WalletMockAPI
  ( handlers
  )where

import           Servant

import qualified Data.OpEnergy.Account.API.V1.Account as API
import           Data.OpEnergy.Account.API.V2.SettleInvoiceRequest
                 ( SettleInvoiceRequest
                 )
import           Data.OpEnergy.Account.API.V2.SettleInvoiceResult
                 ( SettleInvoiceResult
                 )
import           Data.OpEnergy.Account.API.V2.WalletMockAPI (WalletMockAPI)
import           OpEnergy.Account.Server.V1.Class
                 ( AppM, AppT
                 )

import qualified OpEnergy.Account.Server.V2.WalletMockAPI.SettleInvoice
                 as SettleInvoice

-- | see Data.OpEnergy.Account.API.V2.WalletMockAPI for the API definition
handlers :: ServerT WalletMockAPI (AppT Handler)
handlers
  = ( SettleInvoice.settleInvoiceHandler
      :: API.AccountToken
      -> SettleInvoiceRequest
      -> AppM SettleInvoiceResult
    )
