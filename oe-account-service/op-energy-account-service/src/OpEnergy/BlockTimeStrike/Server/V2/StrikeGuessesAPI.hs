{-# LANGUAGE GADTs                      #-}
module OpEnergy.BlockTimeStrike.Server.V2.StrikeGuessesAPI
  ( handlers
  ) where

import           Servant

import           Data.OpEnergy.BlockTime.API.V2.StrikeGuessesAPI
import           Data.OpEnergy.API.V1.Natural(Natural)
import qualified Data.OpEnergy.API.V1.Block as BlockV1

import           OpEnergy.Account.Server.V1.Class (AppM, AppT)

import qualified Data.OpEnergy.BlockTime.API.V2.BlockSpanTimeStrikeGuessesSummary
                 as API
import qualified OpEnergy.BlockTimeStrike.Server.V2.StrikeGuessesAPI.Summary
                 as Summary

handlers
  :: BlockV1.BlockHeight
  -> Natural Int
  -> ServerT StrikeGuessesAPI (AppT Handler)
handlers block mediantime
  =    ( Summary.get block mediantime
         :: AppM API.BlockSpanTimeStrikeGuessesSummary
       )


