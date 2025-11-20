{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DuplicateRecordFields      #-}
module Data.OpEnergy.BlockTime.API.V2.GuessesAPI where

import           Servant.API

import           Data.OpEnergy.API.V1.Natural
import           Data.OpEnergy.API.V1.Positive
import           Data.OpEnergy.Account.API.V1.PagingResult
import           Data.OpEnergy.Account.API.V1.FilterRequest
import           Data.OpEnergy.Account.API.V1.BlockTimeStrikeGuess
                   ( BlockTimeStrikeGuess
                   , BlockTimeStrikeGuessFilter
                   )

import           Data.OpEnergy.BlockTime.API.V2.BlockSpanTimeStrikeGuess

type GuessesAPI
  =  QueryParam'
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

