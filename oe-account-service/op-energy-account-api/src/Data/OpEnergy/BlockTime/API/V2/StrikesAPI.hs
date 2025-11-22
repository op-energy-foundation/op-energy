{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DuplicateRecordFields      #-}
module Data.OpEnergy.BlockTime.API.V2.StrikesAPI where

import           Servant.API

import           Data.OpEnergy.API.V1.Natural
import           Data.OpEnergy.API.V1.Positive
import           Data.OpEnergy.Account.API.V1.PagingResult
import           Data.OpEnergy.Account.API.V1.FilterRequest
import           Data.OpEnergy.Account.API.V1.BlockTimeStrike
                   ( BlockTimeStrike
                   , BlockTimeStrikeFilter
                   )
import qualified Data.OpEnergy.API.V1.Block as BlockV1
import qualified Data.OpEnergy.Account.API.V1.Account as AccountV1
import           Data.OpEnergy.BlockTime.API.V2.BlockSpanTimeStrike

type StrikesAPI
  = "list"
    :> QueryParam'
       '[ Optional
        , Strict
        , Description "defines block span size"
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
                      \'EQ' - equal, 'NEQ' - 'not equal'. \
                      \'sort' field can have those values: 'descend', \
                      \'ascend', 'ascend_guesses_count' or \
                      \'descend_guesses_count'. '*_guesses_count' options \
                      \changes sort orders by guesses count instead of block \
                      \strike id. 'class' field can have those values: \
                      \'guessable', 'outcomeKnown', 'outcomeUnknown'"
        ]
       "filter"
       (FilterRequest BlockTimeStrike BlockTimeStrikeFilter)
    :> Description "returns list of strikes. By default, results are ordered \
                   \by strike id in descending order. \
                   \(ie, from newer to older)"
    :> Get '[JSON] (PagingResult BlockSpanTimeStrike)

  :<|> "create"
    :> Header'
       '[ Required
        , Strict
        , Description "Account token gotten from /login or /register"
        ]
        "Authorization"
        AccountV1.AccountToken -- require authentication
    :> Capture "StrikeBlockHeight" BlockV1.BlockHeight
    :> Capture "StrikeMediantime" (Natural Int)
    :> Description "Creates new time strike by given BlockHeight and strike \
                   \mediantime. Requires authentication. Where: \
                   \BlockHeight - height of the block in the future. It is \
                   \expected, that it should be at least at 12 block in the \
                   \future than current confirmed tip. StrikeMediantime is a \
                   \POSIX time in the future."
    :> Post '[JSON] ()

