{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DuplicateRecordFields      #-}
module Data.OpEnergy.BlockTime.API.V2.StrikeAPI where

import           Servant.API

import           Data.OpEnergy.API.Tags

import           Data.OpEnergy.API.V1.Natural
import           Data.OpEnergy.API.V1.Positive
import qualified Data.OpEnergy.API.V1.Block as BlockV1
import qualified Data.OpEnergy.Account.API.V1.Account as AccountV1
import           Data.OpEnergy.BlockTime.API.V2.BlockSpanTimeStrike
import qualified Data.OpEnergy.BlockTime.API.V2.GuessAPI as GuessAPI
import qualified Data.OpEnergy.BlockTime.API.V2.StrikeGuessesAPI
                 as StrikeGuessesAPI

type StrikeAPI
  =    Capture "StrikeBlockHeight" BlockV1.BlockHeight
    :> Capture "StrikeMediantime" (Natural Int)
    :> QueryParam'
       '[ Optional
        , Strict
        , Description "defines block span size"
        ]
       "spanSize"
       (Positive Int)
    :> Description "returns blockrate time strike defined by strike block \
                   \height and strike mediantime"
    :> Get '[JSON] BlockSpanTimeStrike

  :<|> Header'
       '[ Required
        , Strict
        , Description "Account token gotten from /login or /register"
        ]
        "Authorization"
        AccountV1.AccountToken -- require authentication
    :> Capture "StrikeBlockHeight" BlockV1.BlockHeight
    :> Capture "StrikeMediantime" (Natural Int)
    :> QueryParam'
       '[ Optional
        , Strict
        , Description "defines block span size"
        ]
       "spanSize"
       (Positive Int)
    :> Description "Creates new blockrate time strike by given BlockHeight and \
                   \strike mediantime. Requires authentication. Where: \
                   \- StrikeBlockHeight - height of the block in the future. It is \
                   \expected, that it should be at least at 12 block in the \
                   \future than current confirmed tip.\
                   \ - StrikeMediantime is a POSIX time in the future."
    :> Post '[JSON] BlockSpanTimeStrike

  :<|> Tags "Guess API"
    :> Capture "StrikeBlockHeight" BlockV1.BlockHeight
    :> Capture "StrikeMediantime" (Natural Int)
    :> "guess"
    :> GuessAPI.GuessAPI

  :<|> Tags "Strike guesses API"
    :> Capture "StrikeBlockHeight" BlockV1.BlockHeight
    :> Capture "StrikeMediantime" (Natural Int)
    :> "guesses"
    :> StrikeGuessesAPI.StrikeGuessesAPI

