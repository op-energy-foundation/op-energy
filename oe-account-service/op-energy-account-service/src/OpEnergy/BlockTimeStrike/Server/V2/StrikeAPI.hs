{-# LANGUAGE GADTs                      #-}
module OpEnergy.BlockTimeStrike.Server.V2.StrikeAPI
  ( handlers
  ) where

import           Servant

import           Data.OpEnergy.BlockTime.API.V2.StrikeAPI
import           Data.OpEnergy.BlockTime.API.V2.GuessAPI
import           Data.OpEnergy.API.V1.Natural(Natural)
import           Data.OpEnergy.API.V1.Positive
import qualified Data.OpEnergy.API.V1.Block as BlockV1

import qualified Data.OpEnergy.Account.API.V1.Account as AccountV1
import           OpEnergy.Account.Server.V1.Class (AppM, AppT)

import qualified Data.OpEnergy.BlockTime.API.V2.BlockSpanTimeStrike
                 as API
import qualified OpEnergy.BlockTimeStrike.Server.V2.StrikeAPI.GetStrike
                 as GetStrike
import qualified OpEnergy.BlockTimeStrike.Server.V2.StrikeAPI.Create
                 as Create
import qualified OpEnergy.BlockTimeStrike.Server.V2.GuessAPI
                 as GuessAPI

handlers :: ServerT StrikeAPI (AppT Handler)
handlers
  =     ( GetStrike.getStrikeHandler
          :: BlockV1.BlockHeight
          -> Natural Int
          -> Maybe (Positive Int)
          -> AppM API.BlockSpanTimeStrike
        )

  :<|> ( Create.createHandler
         :: AccountV1.AccountToken -- require authentication
         -> BlockV1.BlockHeight
         -> Natural Int
         -> Maybe (Positive Int)
         -> AppM  API.BlockSpanTimeStrike
       )

  :<|> ( GuessAPI.handlers
         :: BlockV1.BlockHeight
         -> Natural Int
         -> ServerT GuessAPI (AppT Handler)
       )

