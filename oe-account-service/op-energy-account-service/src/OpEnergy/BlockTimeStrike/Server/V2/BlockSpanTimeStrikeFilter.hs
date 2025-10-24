{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
module OpEnergy.BlockTimeStrike.Server.V2.BlockSpanTimeStrikeFilter
  ( buildFilterByClass
  ) where

import           Database.Persist

import           Data.OpEnergy.API.V1.Block
import           Data.OpEnergy.API.V1.Positive( naturalFromPositive, Positive)
import           Data.OpEnergy.Account.API.V1.BlockTimeStrikeFilterClass
import           OpEnergy.BlockTimeStrike.Server.V2.DBModel


-- | this function builds a strike filter with given dynamic variables like
-- current latest confirmed and unconfirmed blocks and etc
buildFilterByClass
  :: Maybe BlockTimeStrikeFilterClass
  -> BlockHeight
  -> BlockHeader
  -> Positive Int
  -> [Filter BlockSpanTimeStrike]
buildFilterByClass
    filterClass
    latestUnconfirmedBlockHeight
    latestConfirmedBlock
    configBlockTimeStrikeGuessMinimumBlockAheadCurrentTip =
  case filterClass of
    Nothing -> []
    Just BlockTimeStrikeFilterClassGuessable ->
      let
          minimumGuessableBlock = latestUnconfirmedBlockHeight + naturalFromPositive configBlockTimeStrikeGuessMinimumBlockAheadCurrentTip
          minimumGuessableMediantime
            = fromIntegral (blockHeaderMediantime latestConfirmedBlock)
            + 600
              * (naturalFromPositive configBlockTimeStrikeGuessMinimumBlockAheadCurrentTip
                + (latestUnconfirmedBlockHeight - blockHeaderHeight latestConfirmedBlock)
                )
          isStrikeBlockHeightGuessable
            = BlockSpanTimeStrikeBlock >=. minimumGuessableBlock
          isStrikeMediantimeGuessable
            = BlockSpanTimeStrikeMediantime
              >. fromIntegral minimumGuessableMediantime
      in
        [ isStrikeBlockHeightGuessable
        , isStrikeMediantimeGuessable
        ]
    Just BlockTimeStrikeFilterClassOutcomeKnown ->
      let
          strikeOutcomeKnownByBlockHeight
            = BlockSpanTimeStrikeBlock <=. blockHeaderHeight latestConfirmedBlock
          strikeOutcomeKnownByMediantime
            = BlockSpanTimeStrikeMediantime <=. fromIntegral (blockHeaderMediantime latestConfirmedBlock)
      in
        [strikeOutcomeKnownByBlockHeight] ||. [strikeOutcomeKnownByMediantime]
    Just BlockTimeStrikeFilterClassOutcomeUnknown ->
      let
          strikeBlockHeightUnconfirmed
            = BlockSpanTimeStrikeBlock >. blockHeaderHeight latestConfirmedBlock
          strikeMediantimeInFuture
            = BlockSpanTimeStrikeMediantime >.
              fromIntegral (blockHeaderMediantime latestConfirmedBlock)
      in
      [ strikeBlockHeightUnconfirmed
      , strikeMediantimeInFuture
      ]

