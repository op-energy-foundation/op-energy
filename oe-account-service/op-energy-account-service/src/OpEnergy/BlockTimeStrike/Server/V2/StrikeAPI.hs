{-# LANGUAGE GADTs                      #-}
module OpEnergy.BlockTimeStrike.Server.V2.StrikeAPI
  ( handlers
  ) where

import           Servant

import           Data.OpEnergy.BlockTime.API.V2.StrikeAPI
import           Data.OpEnergy.BlockTime.API.V2.GuessAPI
import           Data.OpEnergy.BlockTime.API.V2.StrikeGuessesAPI
import           Data.OpEnergy.API.V1.Natural(Natural)
import           Data.OpEnergy.API.V1.Positive
import qualified Data.OpEnergy.API.V1.Block as BlockV1

import           OpEnergy.Account.Server.V1.Class (AppM, AppT)

import qualified Data.OpEnergy.BlockTime.API.V2.BlockSpanTimeStrike
                 as API
import qualified OpEnergy.BlockTimeStrike.Server.V2.StrikeAPI.GetStrike
                 as GetStrike
import qualified OpEnergy.BlockTimeStrike.Server.V2.GuessAPI
                 as GuessAPI
import qualified OpEnergy.BlockTimeStrike.Server.V2.StrikeGuessesAPI
                 as StrikeGuessesAPI

handlers :: ServerT StrikeAPI (AppT Handler)
handlers
  =     ( GetStrike.getStrikeHandler
          :: BlockV1.BlockHeight
          -> Natural Int
          -> Maybe (Positive Int)
          -> AppM API.BlockSpanTimeStrike
        )

  :<|> ( GuessAPI.handlers
         :: BlockV1.BlockHeight
         -> Natural Int
         -> ServerT GuessAPI (AppT Handler)
       )

  :<|> ( StrikeGuessesAPI.handlers
         :: BlockV1.BlockHeight
         -> Natural Int
         -> ServerT StrikeGuessesAPI (AppT Handler)
       )

