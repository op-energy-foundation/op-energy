{-# LANGUAGE GADTs                      #-}
module OpEnergy.BlockTimeStrike.Server.V2.StrikesAPI
  ( handlers
  ) where

import           Servant

import           Data.OpEnergy.BlockTime.API.V2.StrikesAPI
import           Data.OpEnergy.API.V1.Natural(Natural)
import           Data.OpEnergy.API.V1.Positive

import qualified Data.OpEnergy.Account.API.V1.BlockTimeStrike as API
import qualified Data.OpEnergy.Account.API.V1.FilterRequest as API
import qualified Data.OpEnergy.Account.API.V1.PagingResult as API
import qualified Data.OpEnergy.Account.API.V1.Account
                 as AccountV1
import           OpEnergy.Account.Server.V1.Class (AppM, AppT)

import qualified Data.OpEnergy.BlockTime.API.V2.BlockSpanTimeStrike
                 as API
import qualified Data.OpEnergy.BlockTime.API.V2.BlockSpanTimeStrikeGuess
                 as API
import qualified Data.OpEnergy.BlockTime.API.V2.BlockSpanTimeStrikeGuessFilter
                 as API
import qualified OpEnergy.BlockTimeStrike.Server.V2.StrikesAPI.GetStrikes
                 as GetStrikes
import qualified OpEnergy.BlockTimeStrike.Server.V2.StrikesAPI.GetStrikesGuesses
                 as GetStrikesGuesses

handlers :: ServerT StrikesAPI (AppT Handler)
handlers
  = ( GetStrikes.getStrikesHandler
          :: Maybe (Positive Int)
          -> Maybe (Natural Int)
          -> Maybe (API.FilterRequest API.BlockTimeStrike API.BlockTimeStrikeFilter)
          -> AppM (API.PagingResult API.BlockSpanTimeStrike)
    )

  :<|> ( GetStrikesGuesses.getStrikesGuessesHandler
         :: AccountV1.AccountToken
         -> Maybe (Positive Int)
         -> Maybe (Natural Int)
         -> Maybe (API.FilterRequest API.BlockSpanTimeStrikeGuess API.BlockSpanTimeStrikeGuessFilter)
         -> AppM (API.PagingResult API.BlockSpanTimeStrikeGuess)
       )

