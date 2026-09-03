{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
module Data.OpEnergy.Account.API.V2.InternalBalanceAPI
  ( InternalBalanceAPI
  , DeductBalanceAPI
  , CreditBalanceAPI
  ) where

import           Data.Text                  (Text)
import           Servant.API

import           Data.OpEnergy.Account.API.V2.BalanceAdjustRequest
                 ( BalanceAdjustRequest
                 )
import           Data.OpEnergy.Account.API.V2.BalanceAdjustResult
                 ( BalanceAdjustResult
                 )

-- | deduct endpoint spec — the single source of truth
type DeductBalanceAPI
  = Header'
       '[ Required
        , Strict
        , Description "shared secret identifying an internal, \
                      \service-to-service caller"
        ]
       "X-Internal-Service-Secret"
       Text
    :> ReqBody '[JSON] BalanceAdjustRequest
    :> Description "Atomically deducts amountSats from the given \
                   \account's balance, failing (400) if it would go \
                   \negative. Internal-only."
    :> Post '[JSON] BalanceAdjustResult

-- | credit endpoint spec — the single source of truth
type CreditBalanceAPI
  = Header'
       '[ Required
        , Strict
        , Description "shared secret identifying an internal, \
                      \service-to-service caller"
        ]
       "X-Internal-Service-Secret"
       Text
    :> ReqBody '[JSON] BalanceAdjustRequest
    :> Description "Unconditionally credits amountSats to the given \
                   \account's balance. Internal-only."
    :> Post '[JSON] BalanceAdjustResult

-- | internal, shared-secret-gated endpoints for adjusting balance.
-- Never exposed to browser clients -- nginx must block
-- /api/v2/account/internal/*
type InternalBalanceAPI
  = "deduct" :> DeductBalanceAPI
  :<|> "credit" :> CreditBalanceAPI
