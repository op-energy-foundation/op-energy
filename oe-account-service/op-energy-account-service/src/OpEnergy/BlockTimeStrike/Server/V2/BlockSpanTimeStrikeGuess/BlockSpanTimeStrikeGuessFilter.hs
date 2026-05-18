{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GADTs                      #-}
module OpEnergy.BlockTimeStrike.Server.V2.BlockSpanTimeStrikeGuess.BlockSpanTimeStrikeGuessFilter
  where

import qualified Data.List as List

import           Database.Persist
import           Database.Persist.Pagination

import           Data.OpEnergy.Account.API.V1.FilterRequest
import qualified Data.OpEnergy.BlockTime.API.V2.BlockSpanTimeStrikeGuessFilter
                 as BlockSpanTimeStrikeGuessFilter

import qualified OpEnergy.BlockTimeStrike.Server.V1.SlowFast as SlowFast
import           OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrike
import           OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeGuess

instance BuildFilter BlockTimeStrike
    BlockSpanTimeStrikeGuessFilter.BlockSpanTimeStrikeGuessFilter
    where
  sortOrder (filter, _) = maybe Descend
                            BlockSpanTimeStrikeGuessFilter.sortOrderFromStrikeSortOrder
                            (BlockSpanTimeStrikeGuessFilter.sort filter)
  buildFilter ( BlockSpanTimeStrikeGuessFilter.BlockSpanTimeStrikeGuessFilter
                mStrikeMediantimeGTE
                mStrikeMediantimeLTE
                mStrikeMediantimeEQ
                mStrikeMediantimeNEQ
                mStrikeBlockHeightGTE
                mStrikeBlockHeightLTE
                mStrikeBlockHeightEQ
                mStrikeBlockHeightNEQ
                _mObservedBlockHashEQ
                _mObservedBlockHashNEQ
                _mObservedResultEQ
                _mObservedResultNEQ
                _mGuessEQ
                _mGuessNEQ
                _mSort
                _mClass
                _mLinesPerPage
              , _
              ) = List.concat
        -- strike block height
    [ maybe [] (\v -> [ BlockTimeStrikeBlock >=. v]) mStrikeBlockHeightGTE
    , maybe [] (\v -> [ BlockTimeStrikeBlock <=. v]) mStrikeBlockHeightLTE
    , maybe [] (\v -> [ BlockTimeStrikeBlock ==. v]) mStrikeBlockHeightEQ
    , maybe [] (\v -> [ BlockTimeStrikeBlock !=. v]) mStrikeBlockHeightNEQ
    , maybe [] (\v -> [ BlockTimeStrikeStrikeMediantime >=. v]) mStrikeMediantimeGTE
    , maybe [] (\v -> [ BlockTimeStrikeStrikeMediantime <=. v]) mStrikeMediantimeLTE
    , maybe [] (\v -> [ BlockTimeStrikeStrikeMediantime ==. v]) mStrikeMediantimeEQ
    , maybe [] (\v -> [ BlockTimeStrikeStrikeMediantime !=. v]) mStrikeMediantimeNEQ
    ]

instance BuildFilter BlockTimeStrikeObserved
    BlockSpanTimeStrikeGuessFilter.BlockSpanTimeStrikeGuessFilter
    where
  sortOrder (filter, _) = maybe Descend
                            BlockSpanTimeStrikeGuessFilter.sortOrderFromStrikeSortOrder
                            (BlockSpanTimeStrikeGuessFilter.sort filter)
  buildFilter ( BlockSpanTimeStrikeGuessFilter.BlockSpanTimeStrikeGuessFilter
                _mStrikeMediantimeGTE
                _mStrikeMediantimeLTE
                _mStrikeMediantimeEQ
                _mStrikeMediantimeNEQ
                _mStrikeBlockHeightGTE
                _mStrikeBlockHeightLTE
                _mStrikeBlockHeightEQ
                _mStrikeBlockHeightNEQ
                mObservedBlockHashEQ
                mObservedBlockHashNEQ
                mObservedResultEQ
                mObservedResultNEQ
                _mGuessEQ
                _mGuessNEQ
                _mSort
                _mClass
                _mLinesPerPage
              , _
              ) = List.concat
        -- strike block height
    [ maybe [] (\v -> [ BlockTimeStrikeObservedJudgementBlockHash ==. v])
      mObservedBlockHashEQ
    , maybe [] (\v -> [ BlockTimeStrikeObservedJudgementBlockHash !=. v])
      mObservedBlockHashNEQ
    , maybe [] (\v -> [ BlockTimeStrikeObservedIsFast ==. SlowFast.modelApi v])
      mObservedResultEQ
    , maybe [] (\v -> [ BlockTimeStrikeObservedIsFast !=. SlowFast.modelApi v])
      mObservedResultNEQ
    ]

instance BuildFilter BlockTimeStrikeGuess
    BlockSpanTimeStrikeGuessFilter.BlockSpanTimeStrikeGuessFilter
    where
  sortOrder (filter, _) = maybe Descend BlockSpanTimeStrikeGuessFilter.sortOrderFromStrikeSortOrder
                          (BlockSpanTimeStrikeGuessFilter.sort filter)
  buildFilter ( BlockSpanTimeStrikeGuessFilter.BlockSpanTimeStrikeGuessFilter
                _mStrikeMediantimeGTE
                _mStrikeMediantimeLTE
                _mStrikeMediantimeEQ
                _mStrikeMediantimeNEQ
                _mStrikeBlockHeightGTE
                _mStrikeBlockHeightLTE
                _mStrikeBlockHeightEQ
                _mStrikeBlockHeightNEQ
                _mObservedBlockHashEQ
                _mObservedBlockHashNEQ
                _mObservedResultEQ
                _mObservedResultNEQ
                mGuessEQ
                mGuessNEQ
                _mSort
                _mClass
                _mLinesPerPage
              , _
              ) = List.concat
        -- strike block height
    [ maybe [] (\v -> [ BlockTimeStrikeGuessIsFast ==. SlowFast.modelApi v]) mGuessEQ
    , maybe [] (\v -> [ BlockTimeStrikeGuessIsFast !=. SlowFast.modelApi v]) mGuessNEQ
    ]

