{-- | Endpoint of the mock wallet, which has no payer: it marks an invoice
 - paid, so the wallet can be tested before a lightning node exists.
 -
 - It is left out of the swagger, and the service can only answer it while
 - the mock wallet is in use: a real wallet offers no way to mark an invoice
 - paid, so the call is refused whatever it is given.
 -}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
module Data.OpEnergy.Account.API.V2.WalletMockAPI
  ( WalletMockAPI
  , SettleInvoiceAPI
  ) where

import           Servant.API

import           Data.OpEnergy.Account.API.V1.Account (AccountToken)
import           Data.OpEnergy.Account.API.V2.SettleInvoiceRequest
                 ( SettleInvoiceRequest
                 )
import           Data.OpEnergy.Account.API.V2.SettleInvoiceResult
                 ( SettleInvoiceResult
                 )

-- | marks an invoice of the authenticated account paid
type SettleInvoiceAPI
  = Header'
     '[ Required
      , Strict
      , Description "Account token gotten from /login or /register"
      ]
     "Authorization"
     AccountToken
    :> ReqBody '[JSON] SettleInvoiceRequest
    :> Description "Marks an invoice of the authenticated account paid, as a \
                   \payer would. Served only while the mock wallet is in use."
    :> Post '[JSON] SettleInvoiceResult

-- | everything served under /api/v2/account/wallet/mock
type WalletMockAPI
  = "settle" :> SettleInvoiceAPI
