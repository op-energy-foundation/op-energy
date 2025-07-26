{-# LANGUAGE FlexibleContexts  #-}
module OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeFilter
  ( buildFilterByClass
  ) where


import           Database.Persist


import           Data.OpEnergy.API.V1.Block
import           Data.OpEnergy.API.V1.Positive( naturalFromPositive, Positive)
import           Data.OpEnergy.Account.API.V1.BlockTimeStrike
import           Data.OpEnergy.Account.API.V1.BlockTimeStrikeFilterClass


-- | this function builds a strike filter with given dynamic variables like
-- current latest confirmed and unconfirmed blocks and etc
buildFilterByClass
  :: Maybe BlockTimeStrikeFilterClass
  -> BlockHeight
  -> BlockHeader
  -> Positive Int
  -> [Filter BlockTimeStrike]
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
            = BlockTimeStrikeBlock >=. minimumGuessableBlock
          isStrikeMediantimeGuessable
            = BlockTimeStrikeStrikeMediantime
              >. fromIntegral minimumGuessableMediantime
      in
        [ isStrikeBlockHeightGuessable
        , isStrikeMediantimeGuessable
        ]
    Just BlockTimeStrikeFilterClassOutcomeKnown ->
      let
          strikeOutcomeKnownByBlockHeight
            = BlockTimeStrikeBlock <=. blockHeaderHeight latestConfirmedBlock
          strikeOutcomeKnownByMediantime
            = BlockTimeStrikeStrikeMediantime <=. fromIntegral (blockHeaderMediantime latestConfirmedBlock)
      in
        [strikeOutcomeKnownByBlockHeight] ||. [strikeOutcomeKnownByMediantime]
    Just BlockTimeStrikeFilterClassOutcomeUnknown ->
      let
          strikeBlockHeightUnconfirmed
            = BlockTimeStrikeBlock >. blockHeaderHeight latestConfirmedBlock
          strikeMediantimeInFuture
            = BlockTimeStrikeStrikeMediantime >. fromIntegral (blockHeaderMediantime latestConfirmedBlock)
      in
      [ strikeBlockHeightUnconfirmed
      , strikeMediantimeInFuture
      ]

