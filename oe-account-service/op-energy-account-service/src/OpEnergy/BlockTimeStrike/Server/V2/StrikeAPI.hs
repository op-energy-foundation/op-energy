{-# LANGUAGE GADTs                      #-}
module OpEnergy.BlockTimeStrike.Server.V2.StrikeAPI
  ( handlers
  ) where

import           Servant

import           Data.OpEnergy.BlockTime.API.V2.StrikeAPI
import           Data.OpEnergy.API.V1.Natural(Natural)
import           Data.OpEnergy.API.V1.Positive
import qualified Data.OpEnergy.API.V1.Block as BlockV1

import           OpEnergy.Account.Server.V1.Class (AppM, AppT)

import qualified Data.OpEnergy.BlockTime.API.V2.BlockSpanTimeStrike
                 as API
import qualified OpEnergy.BlockTimeStrike.Server.V2.StrikeAPI.GetStrike
                 as GetStrike

handlers :: ServerT StrikeAPI (AppT Handler)
handlers
  =     ( GetStrike.getStrikeHandler
          :: BlockV1.BlockHeight
          -> Natural Int
          -> Maybe (Positive Int)
          -> AppM API.BlockSpanTimeStrike
        )

