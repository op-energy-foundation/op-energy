{-- | Handler wiring for the Wallet API subset.
 -}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
module OpEnergy.Account.Server.V2.WalletAPI
  ( handlers
  )where

import           Data.Word (Word32)
import           Servant

import qualified Data.OpEnergy.Account.API.V1.Account as API
import           Data.OpEnergy.Account.API.V2.CreateInvoiceRequest
                 ( CreateInvoiceRequest
                 )
import           Data.OpEnergy.Account.API.V2.CreateInvoiceResult
                 ( CreateInvoiceResult
                 )
import           Data.OpEnergy.Account.API.V2.PayInvoiceRequest
                 ( PayInvoiceRequest
                 )
import           Data.OpEnergy.Account.API.V2.PayInvoiceResult
                 ( PayInvoiceResult
                 )
import           Data.OpEnergy.Account.API.V2.WalletAPI (WalletAPI)
import           Data.OpEnergy.Account.API.V2.WalletInfo (WalletInfo)
import           Data.OpEnergy.Account.API.V2.WalletTransactionsResult
                 ( WalletTransactionsResult
                 )
import           OpEnergy.Account.Server.V1.Class
                 ( AppM, AppT
                 )

import qualified OpEnergy.Account.Server.V2.WalletAPI.GetWalletInfo
                 as GetWalletInfo
import qualified OpEnergy.Account.Server.V2.WalletAPI.CreateInvoice
                 as CreateInvoice
import qualified OpEnergy.Account.Server.V2.WalletAPI.PayInvoice
                 as PayInvoice
import qualified OpEnergy.Account.Server.V2.WalletAPI.GetTransactions
                 as GetTransactions

-- | see Data.OpEnergy.Account.API.V2.WalletAPI for the API definition
handlers :: ServerT WalletAPI (AppT Handler)
handlers
  = ( GetWalletInfo.getWalletInfoHandler
      :: API.AccountToken
      -> AppM WalletInfo
    )

  :<|> ( CreateInvoice.createInvoiceHandler
         :: API.AccountToken
         -> CreateInvoiceRequest
         -> AppM CreateInvoiceResult
       )

  :<|> ( PayInvoice.payInvoiceHandler
         :: API.AccountToken
         -> PayInvoiceRequest
         -> AppM PayInvoiceResult
       )

  :<|> ( GetTransactions.getWalletTransactionsHandler
         :: API.AccountToken
         -> Maybe Word32
         -> AppM WalletTransactionsResult
       )
