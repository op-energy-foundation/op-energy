{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DerivingStrategies         #-}
module Data.OpEnergy.BlockTime.API.V2.BlockSpanTimeStrike
  ( BlockSpanTimeStrike(..)
  ) where

import           Data.Swagger
import           Control.Lens
import           GHC.Generics
import           Data.Aeson as Aeson
import qualified Data.Text as Text
import           Data.Time.Clock.POSIX(POSIXTime)

import           Data.Default

import           Data.OpEnergy.API.V1.Block(BlockHash, BlockHeight, defaultBlockHeight)
import           Data.OpEnergy.API.V1.Positive(Positive)
import qualified Data.OpEnergy.API.V1.Positive as Positive
import qualified Data.OpEnergy.API.V1.Hash as BlockHash (defaultHash)
import           Data.OpEnergy.Account.API.V1.FilterRequest()
import           Data.OpEnergy.Account.API.V1.SlowFast
import qualified Data.OpEnergy.API.V1 as BlockSpan


-- | this data type defines data structure, that will be used in API to
-- represent BlockSpanTimeStrike with possible observed result, judgement
-- block's data, span size and possible blockspan headers with nbdr and hashrate
data BlockSpanTimeStrike = BlockSpanTimeStrike
  { block :: BlockHeight
  , mediantime :: POSIXTime
  , creationTime :: POSIXTime
  , spanSize :: Positive Int
  , mBlockSpan:: Maybe BlockSpan.BlockSpanHeadersNbdrHashrate
  , observedResult :: Maybe SlowFast
  , observedBlockMediantime :: Maybe POSIXTime
  , observedBlockHash :: Maybe BlockHash
  , observedBlockHeight :: Maybe BlockHeight
  }
  deriving (Show, Generic)
instance FromJSON BlockSpanTimeStrike
instance ToJSON BlockSpanTimeStrike
instance ToSchema BlockSpanTimeStrike where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.type_ ?~ SwaggerObject
    & mapped.schema.description ?~ Text.unlines
        [ "defines BlockSpanTimeStrike data structure. where:"
        , "- block - block height"
        , "- strikeMediantime - mediantime of a given strike"
        , "- creationTime - time when strike had been created"
        , "- observedResult - if exist, then contains observed result of the strike"
        , "- observedBlockMediantime - contains mediantime of the 'judgement' block"
        , "- observedBlockHash - contains hash of the 'judgement' block"
        , "- observedBlockHeight - contains height of the 'judgement' block."
        , "where:"
        , "- judgement block is the block, that used to calculate result against."
        , "  judgement block is used accordigly to the following rules:"
        , "  assume:"
        , "      1. CONFIRMED_MEDIANTIME(n) - is a mediantime of the confirmed block with height n;"
        , "      2. CONFIRMED_BLOCKHASH(n) - is a hash of the confirmed block with height n;"
        , "      3. latestConfirmedHeight - height of the latest confirmed height we have"
        , "      4. strikeBlockHeight - block height of the strike, ie 'block' field's value"
        , ""
        , "  then we will have following cases:"
        , "    1. strikeBlockHeight > latestConfirmedHeight"
        , "       && strikeMediantime > CONFIRMED_MEDIANTIME(latestConfirmedHeight)"
        , "    2. strikeBlockHeight <= latestConfirmedHeight"
        , "    3. strikeMediantime <= CONFIRMED_MEDIANTIME(latestConfirmedHeight)"
        , ""
        , "  case 1: base case: when result can't be calculated, so observed* fields' values are absent/null"
        , ""
        , "  case 2: when strike's block had been discovered."
        , "    so the judgement block's height here is the same as strike's 'block' field's value. In this case:"
        , "      - observedResult = \"fast\" if strikeMediantime > CONFIRMED_MEDIANTIME(judgementBlockHeight) (ie, when block had been discovered faster than defined by strike), or \"slow\" otherwise"
        , "      - observedBlockHeight = the same as 'block' field's value"
        , "      - observedBlockMediantime = CONFIRMED_MEDIANTIME(judgementBlockHeight)"
        , "      - observedBlockHash = CONFIRMED_BLOCKHASH(judgementBlockHeight)"
        , ""
        , "  case 3: when strike's block hadn't been discovered, but strikeMediantime had been reached"
        , "    so the judgement block's height here is found as:"
        , "      thus CONFIRMED_MEDIANTIME(N-1) < strikeMediantime <= CONFIRMED_MEDIANTIME(judgementBlockHeight) <= CONFIRMED_MEDIANTIME(latestConfirmedHeight)"
        , "      and: n-1 < judgementBlockHeight < latestConfirmedHeight < strikeBlockHeight"
        , "      and: CONFIRMED_MEDIANTIME(strikeBlockHeight) is not exist"
        , "      - observedResult = \"slow\" as block discover is slower than expected by strike"
        , "      - observedBlockHeight = judgementBlockHeight"
        , "      - observedBlockMediantime = CONFIRMED_MEDIANTIME(judgementBlockHeight)"
        , "      - observedBlockHash = CONFIRMED_BLOCKHASH(judgementBlockHeight)"
        ]
    & mapped.schema.example ?~ toJSON defaultBlockSpanTimeStrike
--    TODO: recheck
--    & mapped.schema.required .~
--        [ "block"
--        , "strikeMediantime"
--        , "creationTime"
--        ]
instance Default BlockSpanTimeStrike where
  def = defaultBlockSpanTimeStrike
defaultBlockSpanTimeStrike :: BlockSpanTimeStrike
defaultBlockSpanTimeStrike =  BlockSpanTimeStrike
  { block = defaultBlockHeight
  , mediantime = 2
  , creationTime = 1
  , observedResult = Just def
  , observedBlockMediantime = Just 3
  , observedBlockHash = Just BlockHash.defaultHash
  , observedBlockHeight = Just defaultBlockHeight
  , spanSize = Positive.verifyPositive 24
  , mBlockSpan = Just BlockSpan.defaultBlockSpanHeadersNbdrHashrate
  }

