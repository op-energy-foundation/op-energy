{-# LANGUAGE GADTs                      #-}
module OpEnergy.BlockTimeStrike.Server.V2.BlockSpanTimeStrikeGuessService
  ( handlers
  ) where

import           Servant

import           Data.OpEnergy.API.V1.Positive
import           Data.OpEnergy.API.V1.Natural
import qualified Data.OpEnergy.Account.API.V1.FilterRequest as API
import qualified Data.OpEnergy.Account.API.V1.PagingResult as API
import qualified Data.OpEnergy.Account.API.V1.BlockTimeStrikeGuess
                 as APIV1
import qualified Data.OpEnergy.Account.API.V1.Account
                 as APIV1
import qualified Data.OpEnergy.API.V1.Block
                 as APIV1
import qualified Data.OpEnergy.Account.API.V1.SlowFast
                 as APIV1

import qualified Data.OpEnergy.BlockTime.API.V2.BlockSpanTimeStrikeGuess
                 as API
import           Data.OpEnergy.BlockTime.API.V2.GuessesAPI

import qualified OpEnergy.BlockTimeStrike.Server.V2.BlockSpanTimeStrikeGuessService.GetStrikesGuesses
                 as GetStrikesGuesses
import qualified OpEnergy.BlockTimeStrike.Server.V2.BlockSpanTimeStrikeGuessService.Create
                 as Create
import qualified OpEnergy.BlockTimeStrike.Server.V2.BlockSpanTimeStrikeGuessService.StrikeGuesses
                 as StrikeGuesses
import           OpEnergy.Account.Server.V1.Class (AppM, AppT)

handlers :: ServerT GuessesAPI (AppT Handler)
handlers
  =    (GetStrikesGuesses.getStrikesGuessesHandler
         :: Maybe (Positive Int)
         -> Maybe (Natural Int)
         -> Maybe ( API.FilterRequest
                    APIV1.BlockTimeStrikeGuess
                    APIV1.BlockTimeStrikeGuessFilter
                  )
         -> AppM (API.PagingResult API.BlockSpanTimeStrikeGuess)
       )

  :<|> ( Create.createHandler
         :: APIV1.AccountToken -- require authentication
         -> APIV1.BlockHeight
         -> Natural Int
         -> APIV1.SlowFast
         -> AppM API.BlockSpanTimeStrikeGuess
       )

  :<|> ( StrikeGuesses.getStrikeGuessesHandler
         :: APIV1.BlockHeight
         -> Natural Int
         -> Maybe (Natural Int)
         -> Maybe
            ( API.FilterRequest
              APIV1.BlockTimeStrikeGuess
              APIV1.BlockTimeStrikeGuessFilter
            )
         -> Maybe (Positive Int)
         -> AppM (API.PagingResult API.BlockSpanTimeStrikeGuess)
       )
