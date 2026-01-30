{-# LANGUAGE GADTs                      #-}
module OpEnergy.BlockTimeStrike.Server.V2.GuessAPI
  ( handlers
  ) where

import           Servant

import           Data.OpEnergy.BlockTime.API.V2.GuessAPI
import           Data.OpEnergy.API.V1.Positive
import           Data.OpEnergy.API.V1.Natural(Natural)
import qualified Data.OpEnergy.API.V1.Block as BlockV1

import qualified Data.OpEnergy.Account.API.V1.Account as AccountV1
import           OpEnergy.Account.Server.V1.Class (AppM, AppT)

import qualified Data.OpEnergy.BlockTime.API.V2.BlockSpanTimeStrikeGuess
                 as API
import qualified OpEnergy.BlockTimeStrike.Server.V2.GuessAPI.Get
                 as Get

handlers
  :: BlockV1.BlockHeight
  -> Natural Int
  -> ServerT GuessAPI (AppT Handler)
handlers block mediantime
  =    ( Get.getHandler block mediantime
         :: AccountV1.AccountToken
         -> Maybe (Positive Int)
         -> AppM API.BlockSpanTimeStrikeGuess
       )


