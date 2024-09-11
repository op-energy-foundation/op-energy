{-- | This module defines BlockTimeStrikeWithGuessesCountPublic datatype
 --}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE DeriveGeneric              #-}
module Data.OpEnergy.Account.API.V1.BlockTimeStrikePublic where

import           Data.Swagger
import           Control.Lens
import           GHC.Generics
import           Data.Aeson as A
import           Data.Word
import           Data.Time.Clock.POSIX(POSIXTime)
import qualified Data.Text as Text

import           Data.OpEnergy.API.V1.Block(BlockHash)
import qualified Data.OpEnergy.API.V1.Hash as BlockHash (defaultHash)
import           Data.OpEnergy.API.V1.Block(BlockHeight, defaultBlockHeight)
import           Data.OpEnergy.Account.API.V1.BlockTimeStrike
import           Data.OpEnergy.Account.API.V1.Common
import           Data.Default
import           Data.Proxy(Proxy(..))

-- | this data type defines data structure, that will be used in API to
-- represent BlockTimeStrike with possible observed result and judgement
-- block's data
data BlockTimeStrikePublic = BlockTimeStrikePublic
  { blockTimeStrikePublicBlock :: BlockHeight
  , blockTimeStrikePublicStrikeMediantime :: POSIXTime
  , blockTimeStrikePublicCreationTime :: POSIXTime
  , blockTimeStrikePublicObservedResult :: Maybe SlowFast
  , blockTimeStrikePublicObservedBlockMediantime :: Maybe POSIXTime
  , blockTimeStrikePublicObservedBlockHash :: Maybe BlockHash
  , blockTimeStrikePublicObservedBlockHeight :: Maybe BlockHeight
  }
  deriving (Show, Generic)
instance FromJSON BlockTimeStrikePublic where
  parseJSON = commonParseJSON
instance ToJSON BlockTimeStrikePublic where
  -- alter serialization names
  toJSON = commonToJSON genericToJSON
  toEncoding = commonToJSON genericToEncoding
instance ToSchema BlockTimeStrikePublic where
  declareNamedSchema v = do
    blockSchema <- declareSchemaRef (def2 $! blockTimeStrikePublicBlock $! def1 v)
    strikeMediantimeSchema <- declareSchemaRef (def2 $! blockTimeStrikePublicStrikeMediantime $! def1 v)
    creationTimeSchema <- declareSchemaRef (def2 $! blockTimeStrikePublicCreationTime $! def1 v)
    observedResultSchema <- declareSchemaRef (def2 $! blockTimeStrikePublicObservedResult $! def1 v)
    observedBlockMediantimeSchema <- declareSchemaRef (def2 $! blockTimeStrikePublicObservedBlockMediantime $! def1 v)
    observedBlockHashSchema <- declareSchemaRef (def2 $! blockTimeStrikePublicObservedBlockHash $! def1 v)
    observedBlockHeightSchema <- declareSchemaRef (def2 $! blockTimeStrikePublicObservedBlockHeight $! def1 v)
    return $ NamedSchema (Just "BlockTimeStrikePublic") $ mempty
      & type_ ?~ SwaggerObject
      & description ?~(Text.unlines
        [ "defines BlockTimeStrike data structure. where:"
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
        ])
      & example ?~ toJSON defaultBlockTimeStrikePublic
      & properties .~
        [ ( "block", blockSchema)
        , ( "strikeMediantime", strikeMediantimeSchema)
        , ( "creationTime", creationTimeSchema)
        , ( "observedResult", observedResultSchema)
        , ( "observedBlockMediantime", observedBlockMediantimeSchema)
        , ( "observedBlockHash", observedBlockHashSchema)
        , ( "observedBlockHeight", observedBlockHeightSchema)
        ]
      & required .~
        [ "block"
        , "strikeMediantime"
        , "creationTime"
        ]
    where
      def2 :: b -> Proxy b
      def2 _ = Proxy
      def1 :: Default a => Proxy a -> a
      def1 _ = def
instance Default BlockTimeStrikePublic where
  def = defaultBlockTimeStrikePublic
defaultBlockTimeStrikePublic :: BlockTimeStrikePublic
defaultBlockTimeStrikePublic =  BlockTimeStrikePublic
  { blockTimeStrikePublicBlock = defaultBlockHeight
  , blockTimeStrikePublicStrikeMediantime = 2
  , blockTimeStrikePublicCreationTime = 1
  , blockTimeStrikePublicObservedResult = Just defaultSlowFast
  , blockTimeStrikePublicObservedBlockMediantime = Just 3
  , blockTimeStrikePublicObservedBlockHash = Just BlockHash.defaultHash
  , blockTimeStrikePublicObservedBlockHeight = Just defaultBlockHeight
  }

data BlockTimeStrikeWithGuessesCountPublic = BlockTimeStrikeWithGuessesCountPublic
  { blockTimeStrikeWithGuessesCountPublicStrike :: BlockTimeStrikePublic
    -- ^ past strike
  , blockTimeStrikeWithGuessesCountPublicGuessesCount :: Word32
    -- ^ amount of guesses
  }
  deriving (Show, Generic)
instance FromJSON BlockTimeStrikeWithGuessesCountPublic where
  parseJSON = commonParseJSON
instance ToJSON BlockTimeStrikeWithGuessesCountPublic where
  -- alter serialization names
  toJSON = commonToJSON genericToJSON
  toEncoding = commonToJSON genericToEncoding
instance ToSchema BlockTimeStrikeWithGuessesCountPublic where
  declareNamedSchema v = do
    blockTimeStrikeWithGuessesCountPublicStrikeSchema <-
      declareSchemaRef (def2 $! blockTimeStrikeWithGuessesCountPublicStrike $! def1 v)
    blockTimeStrikeWithGuessesCountPublicGuessesCountSchema <-
      declareSchemaRef (def2 $! blockTimeStrikeWithGuessesCountPublicGuessesCount $! def1 v)
    return $ NamedSchema (Just "BlockTimeStrikeWithGuessesCountPublic") $ mempty
      & type_ ?~ SwaggerObject
      & description ?~ "This object defines strike and it's guesses count"
      & example ?~ toJSON defaultBlockTimeStrikeWithGuessesCountPublic
      & properties .~
        [ ( "strike", blockTimeStrikeWithGuessesCountPublicStrikeSchema)
        , ( "guessesCount", blockTimeStrikeWithGuessesCountPublicGuessesCountSchema)
        ]
      & required .~
        [ "strike"
        , "guessesCount"
        ]
    where
      def2 :: b -> Proxy b
      def2 _ = Proxy
      def1 :: Default a => Proxy a -> a
      def1 _ = def


instance Default BlockTimeStrikeWithGuessesCountPublic where
  def = defaultBlockTimeStrikeWithGuessesCountPublic
defaultBlockTimeStrikeWithGuessesCountPublic :: BlockTimeStrikeWithGuessesCountPublic
defaultBlockTimeStrikeWithGuessesCountPublic = BlockTimeStrikeWithGuessesCountPublic
  { blockTimeStrikeWithGuessesCountPublicStrike = defaultBlockTimeStrikePublic
  , blockTimeStrikeWithGuessesCountPublicGuessesCount = 0
  }
