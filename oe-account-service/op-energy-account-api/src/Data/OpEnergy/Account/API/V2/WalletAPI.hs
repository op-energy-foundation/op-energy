{-- | Wallet endpoints of the account service: everything a client needs to
 - see its balance, be paid, pay someone and read its history.
 -
 - A client never talks to the lightning wallet itself: it calls these, and
 - this service talks to the wallet.
 -}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
module Data.OpEnergy.Account.API.V2.WalletAPI
  ( WalletAPI
  , WalletInfoAPI
  , CreateInvoiceAPI
  , PayInvoiceAPI
  , WalletTransactionsAPI
  ) where

import           Data.Word (Word32)
import           Servant.API

import           Data.OpEnergy.Account.API.V1.Account (AccountToken)
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
import           Data.OpEnergy.Account.API.V2.WalletInfo (WalletInfo)
import           Data.OpEnergy.Account.API.V2.WalletTransactionsResult
                 ( WalletTransactionsResult
                 )

-- | the account token header every wallet endpoint requires
type WalletAuth
  = Header'
     '[ Required
      , Strict
      , Description "Account token gotten from /login or /register"
      ]
     "Authorization"
     AccountToken

-- | balance and what the wallet currently allows
type WalletInfoAPI
  = WalletAuth
    :> Description "Returns the balance of the authenticated account, which \
                   \wallet is in use and the amounts it accepts."
    :> Get '[JSON] WalletInfo

-- | an invoice, which someone else can pay to this account
type CreateInvoiceAPI
  = WalletAuth
    :> ReqBody '[JSON] CreateInvoiceRequest
    :> Description "Creates a lightning invoice to be paid to the \
                   \authenticated account."
    :> Post '[JSON] CreateInvoiceResult

-- | pays an invoice out of this account's balance
type PayInvoiceAPI
  = WalletAuth
    :> ReqBody '[JSON] PayInvoiceRequest
    :> Description "Pays the given invoice from the authenticated account's \
                   \balance, failing if the balance does not cover it."
    :> Post '[JSON] PayInvoiceResult

-- | every change of this account's balance, newest first
type WalletTransactionsAPI
  = WalletAuth
    :> QueryParam' '[Optional, Strict, Description "page to return, 0 is the newest"] "page" Word32
    :> Description "Returns the history of the authenticated account's \
                   \balance, newest first."
    :> Get '[JSON] WalletTransactionsResult

-- | everything served under /api/v2/account/wallet
type WalletAPI
  = "balance" :> WalletInfoAPI
  :<|> "invoice" :> CreateInvoiceAPI
  :<|> "pay" :> PayInvoiceAPI
  :<|> "transactions" :> WalletTransactionsAPI
