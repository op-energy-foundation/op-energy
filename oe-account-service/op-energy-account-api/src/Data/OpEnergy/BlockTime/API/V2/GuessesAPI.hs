{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DuplicateRecordFields      #-}
module Data.OpEnergy.BlockTime.API.V2.GuessesAPI where

import           Servant.API

import           Data.OpEnergy.API.V1.Natural
import           Data.OpEnergy.API.V1.Positive
import           Data.OpEnergy.Account.API.V1.Account
import           Data.OpEnergy.Account.API.V1.PagingResult
import           Data.OpEnergy.Account.API.V1.FilterRequest
import           Data.OpEnergy.Account.API.V1.BlockTimeStrikeGuess
                   ( BlockTimeStrikeGuess
                   , BlockTimeStrikeGuessFilter
                   )
import qualified Data.OpEnergy.API.V1.Block as BlockV1
import qualified Data.OpEnergy.Account.API.V1.SlowFast as V1

import           Data.OpEnergy.BlockTime.API.V2.BlockSpanTimeStrikeGuess

type GuessesAPI
  = "list"
    :> QueryParam'
       '[ Optional
        , Strict
        , Description "defines strikes' block span size"
        ]
       "spanSize"
       (Positive Int)
    :> QueryParam'
       '[ Optional
        , Strict
        , Description "defines page count to get"
        ]
       "page"
       (Natural Int)
    :> QueryParam'
       '[ Optional
        , Strict
        , Description "possible filter as a string in JSON format. you can \
                      \pass any combination of it's unique fields to build a \
                      \filter. Available filter options are listed in the \
                      \format of current field. Meaning of fields' suffixes: \
                      \'GTE' - 'great-than-or-equal', \
                      \'LTE'- 'less-than-or-equal', \
                      \'EQ' - equal, \
                      \'NEQ' - 'not equal'. \
                      \'sort' field can have those values: 'descend', \
                      \'ascend'. 'class' field can have those values: \
                      \'guessable', 'outcomeKnown', 'outcomeUnknown'"
        ]
       "filter"
       ( FilterRequest
           BlockTimeStrikeGuess
           BlockTimeStrikeGuessFilter
       )
    :> Description "returns guesses for the given blocktime strike. By \
                   \default, results are order by id in decending order \
                   \(from new to old)"
    :> Get '[JSON] (PagingResult BlockSpanTimeStrikeGuess)

  :<|> "create"
    :> Header'
       '[ Required
        , Strict
        , Description "Account token gotten from /login or /register"
        ]
       "Authorization"
       AccountToken -- require authentication
    :> Capture "StrikeBlockHeight" BlockV1.BlockHeight
    :> Capture "StrikeMediantime" (Natural Int)
    :> Capture "guess" V1.SlowFast
    :> Description "creates a guess for the given future time strike. Requires \
                   \authentication."
    :> Post '[JSON] BlockSpanTimeStrikeGuess

  :<|> "strike"
    :> "guesses"
    :> Capture "StrikeBlockHeight" BlockV1.BlockHeight
    :> Capture "StrikeMediantime" (Natural Int)
    :> QueryParam'
       '[ Optional
        , Strict
        , Description "defines page count to get"
        ]
       "page"
       (Natural Int)
    :> QueryParam'
       '[ Optional
        , Strict
        , Description "possible filter as a string in JSON format. you can \
                      \pass any combination of it's unique fields to build a \
                      \filter. Available filter options are listed in the \
                      \format of current field. Meaning of fields' suffixes: \
                      \'GTE' - 'great-than-or-equal', \
                      \'LTE'- 'less-than-or-equal', \
                      \'EQ' - equal, 'NEQ' - 'not equal'. \
                      \'sort' field can have those values: 'descend', 'ascend'."
        ]
       "filter"
       ( FilterRequest
         BlockTimeStrikeGuess
         BlockTimeStrikeGuessFilter
       )
    :> QueryParam'
       '[ Optional
        , Strict
        , Description "defines strikes' block span size"
        ]
       "spanSize"
       (Positive Int)
    :> Description "returns guesses for the given blocktime strike. By \
                   \default, results are order by id in decending order (from \
                   \new to old)"
    :> Get '[JSON] (PagingResult BlockSpanTimeStrikeGuess)


