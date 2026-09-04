{-# LANGUAGE GADTs                      #-}
module OpEnergy.Account.Server.V2.InternalBalanceAPI
  ( handlers
  ) where

import           Servant

import           Data.Text(Text)
import           Data.OpEnergy.Account.API.V2.InternalBalanceAPI
import           Data.OpEnergy.Account.API.V2.BalanceAdjustRequest (BalanceAdjustRequest)
import           Data.OpEnergy.Account.API.V2.BalanceAdjustResult (BalanceAdjustResult)
import           OpEnergy.Account.Server.V1.Class (AppM, AppT)

import qualified OpEnergy.Account.Server.V2.AccountService.DeductBalance
                 as DeductBalance
import qualified OpEnergy.Account.Server.V2.AccountService.CreditBalance
                 as CreditBalance

handlers :: ServerT InternalBalanceAPI (AppT Handler)
handlers
  = ( DeductBalance.deductBalanceHandler
      :: Text
      -> BalanceAdjustRequest
      -> AppM BalanceAdjustResult
    )

  :<|> ( CreditBalance.creditBalanceHandler
         :: Text
         -> BalanceAdjustRequest
         -> AppM BalanceAdjustResult
       )
