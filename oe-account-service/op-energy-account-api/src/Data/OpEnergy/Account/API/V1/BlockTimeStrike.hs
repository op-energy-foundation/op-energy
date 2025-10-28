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
module Data.OpEnergy.Account.API.V1.BlockTimeStrike
  ( BlockTimeStrike(..)
  , BlockTimeStrikeWithGuessesCount(..)
  , BlockTimeStrikeFilter(..)
  , sortOrderFromStrikeSortOrder
  , StrikeSortOrder(..)
  ) where

import           Data.Swagger
import           Control.Lens
import           GHC.Generics
import           Data.Aeson as Aeson
import           Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Time.Clock.POSIX(POSIXTime)
import qualified Data.List as List
import qualified Data.ByteString.Lazy as BS
import           Data.Word(Word32)

import           Servant.API(ToHttpApiData(..), FromHttpApiData(..))
import           Database.Persist.Pagination
import           Data.Proxy
import           Data.Default

import           Data.OpEnergy.API.V1.Block(BlockHash, BlockHeight, defaultBlockHeight)
import           Data.OpEnergy.Account.API.V1.Account
import           Data.OpEnergy.API.V1.Positive(Positive)
import qualified Data.OpEnergy.API.V1.Hash as BlockHash (defaultHash)
import           Data.OpEnergy.Account.API.V1.FilterRequest()
import           Data.OpEnergy.Account.API.V1.Common
import           Data.OpEnergy.Account.API.V1.BlockTimeStrikeFilterClass
import           Data.OpEnergy.Account.API.V1.SlowFast

-- | this data type defines data structure, that will be used in API to
-- represent BlockTimeStrike with possible observed result and judgement
-- block's data
data BlockTimeStrike = BlockTimeStrike
  { blockTimeStrikeBlock :: BlockHeight
  , blockTimeStrikeStrikeMediantime :: POSIXTime
  , blockTimeStrikeCreationTime :: POSIXTime
  , blockTimeStrikeObservedResult :: Maybe SlowFast
  , blockTimeStrikeObservedBlockMediantime :: Maybe POSIXTime
  , blockTimeStrikeObservedBlockHash :: Maybe BlockHash
  , blockTimeStrikeObservedBlockHeight :: Maybe BlockHeight
  }
  deriving (Eq, Show, Generic)
instance FromJSON BlockTimeStrike where
  parseJSON = commonParseJSON
instance ToJSON BlockTimeStrike where
  -- alter serialization names
  toJSON = commonToJSON genericToJSON
  toEncoding = commonToJSON genericToEncoding
instance ToSchema BlockTimeStrike where
  declareNamedSchema proxy = genericDeclareNamedSchema (commonSchemaOptions (def1 proxy)) proxy
    & mapped.schema.type_ ?~ SwaggerObject
    & mapped.schema.description ?~ (Text.unlines
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
    & mapped.schema.example ?~ toJSON defaultBlockTimeStrike
    & mapped.schema.required .~
        [ "block"
        , "strikeMediantime"
        , "creationTime"
        ]
    where
      def1 :: Default a => Proxy a -> a
      def1 _ = def
instance Default BlockTimeStrike where
  def = defaultBlockTimeStrike
defaultBlockTimeStrike :: BlockTimeStrike
defaultBlockTimeStrike =  BlockTimeStrike
  { blockTimeStrikeBlock = defaultBlockHeight
  , blockTimeStrikeStrikeMediantime = 2
  , blockTimeStrikeCreationTime = 1
  , blockTimeStrikeObservedResult = Just def
  , blockTimeStrikeObservedBlockMediantime = Just 3
  , blockTimeStrikeObservedBlockHash = Just BlockHash.defaultHash
  , blockTimeStrikeObservedBlockHeight = Just defaultBlockHeight
  }

data BlockTimeStrikeWithGuessesCount = BlockTimeStrikeWithGuessesCount
  { blockTimeStrikeWithGuessesCountStrike :: BlockTimeStrike
    -- ^ past strike
  , blockTimeStrikeWithGuessesCountGuessesCount :: Word32
    -- ^ amount of guesses
  }
  deriving (Show, Generic)
instance FromJSON BlockTimeStrikeWithGuessesCount where
  parseJSON = commonParseJSON
instance ToJSON BlockTimeStrikeWithGuessesCount where
  -- alter serialization names
  toJSON = commonToJSON genericToJSON
  toEncoding = commonToJSON genericToEncoding
instance ToSchema BlockTimeStrikeWithGuessesCount where
  declareNamedSchema proxy = genericDeclareNamedSchema (commonSchemaOptions (def1 proxy)) proxy
    & mapped.schema.type_ ?~ SwaggerObject
    & mapped.schema.description ?~ "This object defines strike and it's guesses count"
    & mapped.schema.example ?~ toJSON defaultBlockTimeStrikeWithGuessesCount
    & mapped.schema.required .~
      [ "strike"
      , "guessesCount"
      ]
    where
      def1 :: Default a => Proxy a -> a
      def1 _ = def


instance Default BlockTimeStrikeWithGuessesCount where
  def = defaultBlockTimeStrikeWithGuessesCount
defaultBlockTimeStrikeWithGuessesCount :: BlockTimeStrikeWithGuessesCount
defaultBlockTimeStrikeWithGuessesCount = BlockTimeStrikeWithGuessesCount
  { blockTimeStrikeWithGuessesCountStrike = defaultBlockTimeStrike
  , blockTimeStrikeWithGuessesCountGuessesCount = 0
  }
data BlockTimeStrikeFilter = BlockTimeStrikeFilter
  { blockTimeStrikeFilterStrikeMediantimeGTE        :: Maybe POSIXTime
  , blockTimeStrikeFilterStrikeMediantimeLTE        :: Maybe POSIXTime
  , blockTimeStrikeFilterStrikeMediantimeEQ         :: Maybe POSIXTime
  , blockTimeStrikeFilterStrikeMediantimeNEQ        :: Maybe POSIXTime
  , blockTimeStrikeFilterStrikeBlockHeightGTE       :: Maybe BlockHeight
  , blockTimeStrikeFilterStrikeBlockHeightLTE       :: Maybe BlockHeight
  , blockTimeStrikeFilterStrikeBlockHeightEQ        :: Maybe BlockHeight
  , blockTimeStrikeFilterStrikeBlockHeightNEQ       :: Maybe BlockHeight
  , blockTimeStrikeFilterObservedBlockHashEQ        :: Maybe BlockHash
  , blockTimeStrikeFilterObservedBlockHashNEQ       :: Maybe BlockHash
  , blockTimeStrikeFilterObservedResultEQ           :: Maybe SlowFast
  , blockTimeStrikeFilterObservedResultNEQ          :: Maybe SlowFast
  , blockTimeStrikeFilterSort                       :: Maybe StrikeSortOrder
  , blockTimeStrikeFilterClass                      :: Maybe BlockTimeStrikeFilterClass
  , blockTimeStrikeFilterLinesPerPage               :: Maybe (Positive Int)
  }
  deriving (Eq, Show, Generic)
instance ToSchema BlockTimeStrikeFilter where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ (Text.unlines
      [ "This is the example of the public filter available for BlockTimeStrike"
      , "each "
      ])
    & mapped.schema.example ?~ toJSON defaultBlockTimeStrike
instance ToJSON BlockTimeStrikeFilter where
  toJSON = commonToJSON genericToJSON
  toEncoding = commonToJSON genericToEncoding
instance FromJSON BlockTimeStrikeFilter where
  parseJSON = commonParseJSON
instance Default BlockTimeStrikeFilter where
  def = defaultBlockTimeStrikeFilter

defaultBlockTimeStrikeFilter :: BlockTimeStrikeFilter
defaultBlockTimeStrikeFilter = BlockTimeStrikeFilter
  { blockTimeStrikeFilterStrikeMediantimeGTE   = Just 1
  , blockTimeStrikeFilterStrikeMediantimeLTE   = Just 1
  , blockTimeStrikeFilterStrikeMediantimeEQ    = Just 1
  , blockTimeStrikeFilterStrikeMediantimeNEQ   = Just 2
  , blockTimeStrikeFilterStrikeBlockHeightGTE  = Just 1
  , blockTimeStrikeFilterStrikeBlockHeightLTE  = Just 1
  , blockTimeStrikeFilterStrikeBlockHeightEQ   = Just 1
  , blockTimeStrikeFilterStrikeBlockHeightNEQ  = Just 1
  , blockTimeStrikeFilterObservedBlockHashEQ   = Just BlockHash.defaultHash
  , blockTimeStrikeFilterObservedBlockHashNEQ  = Just BlockHash.defaultHash
  , blockTimeStrikeFilterObservedResultEQ      = Just Slow
  , blockTimeStrikeFilterObservedResultNEQ     = Just Fast
  , blockTimeStrikeFilterSort                  = Just StrikeSortOrderDescend
  , blockTimeStrikeFilterClass                 = Just BlockTimeStrikeFilterClassGuessable
  , blockTimeStrikeFilterLinesPerPage          = Just 100
  }

data BlockTimeStrikeObserved =  BlockTimeStrikeObserved
  { blockTimeStrikeObservedBlockHash :: Maybe BlockHash
  , blockTimeStrikeObservedCreationTime :: POSIXTime
  , blockTimeStrikeObservedResult :: SlowFast
  , blockTimeStrikeObservedBlockMediantime :: Maybe POSIXTime
  }
  deriving (Eq, Show, Generic)
defaultBlockTimeStrikeObserved :: BlockTimeStrikeObserved
defaultBlockTimeStrikeObserved =  BlockTimeStrikeObserved
  { blockTimeStrikeObservedBlockHash = Just BlockHash.defaultHash
  , blockTimeStrikeObservedCreationTime = defaultPOSIXTime
  , blockTimeStrikeObservedResult = Slow
  , blockTimeStrikeObservedBlockMediantime = Just 1
  }
instance ToSchema BlockTimeStrikeObserved where
  declareNamedSchema proxy = genericDeclareNamedSchema (commonSchemaOptions (def1 proxy)) proxy
    & mapped.schema.type_ ?~ SwaggerObject
    & mapped.schema.example ?~ toJSON defaultBlockTimeStrikeObserved
    & mapped.schema.required .~
      [ "blockHash"
      , "creationTime"
      , "result"
      , "blockMediantime"
      ]
    where
      def1 :: Default a => Proxy a -> a
      def1 _ = def
instance ToJSON BlockTimeStrikeObserved where
  toJSON = commonToJSON genericToJSON
  toEncoding = commonToJSON genericToEncoding
instance FromJSON BlockTimeStrikeObserved where
  parseJSON = commonParseJSON
instance Default BlockTimeStrikeObserved where
  def = defaultBlockTimeStrikeObserved

instance ToParamSchema BlockTimeStrikeFilter where
  toParamSchema v = mempty
    & type_ ?~ SwaggerString
    & format ?~ ( Text.unlines
                 $ List.map (<>",")
                 $ Text.splitOn ","
                 $ Text.decodeUtf8
                 $ BS.toStrict
                 $ encode
                 $ def1 v
                )
    where
      def1 :: Default a => Proxy a-> a
      def1 = def
instance ToHttpApiData BlockTimeStrikeFilter where
  toUrlPiece v = toUrlPiece $ Text.decodeUtf8 $ BS.toStrict $ encode v
  toQueryParam v = toQueryParam $ Text.decodeUtf8 $ BS.toStrict $ encode v
instance FromHttpApiData BlockTimeStrikeFilter where
  parseUrlPiece v = case Aeson.eitherDecodeStrict (Text.encodeUtf8 v) of
    Left some -> Left (Text.pack some)
    Right some -> Right some
  parseQueryParam v = case Aeson.eitherDecodeStrict (Text.encodeUtf8 v) of
    Left some -> Left (Text.pack some)
    Right some -> Right some

-- | we need to use separate sort order as we can sort strikes by guesses count
data StrikeSortOrder
  = StrikeSortOrderAscend
  | StrikeSortOrderDescend
  | StrikeSortOrderAscendGuessesCount
  | StrikeSortOrderDescendGuessesCount
  deriving (Eq, Generic, Bounded, Enum)

instance ToJSON StrikeSortOrder where
  toJSON = toJSON . Text.pack . show
instance FromJSON StrikeSortOrder where
  parseJSON = withText "SortOrder" $! pure . verifyStrikeSortOrder
instance ToSchema StrikeSortOrder where
  declareNamedSchema _ = pure $ NamedSchema (Just "SortOrder") $ mempty
    & type_ ?~ SwaggerString
instance ToParamSchema StrikeSortOrder where
  toParamSchema _ = mempty
    & type_ ?~ SwaggerString
    & enum_ ?~ (map toJSON $ enumFrom Ascend)
instance ToHttpApiData StrikeSortOrder where
  toUrlPiece = Text.pack . show
instance FromHttpApiData StrikeSortOrder where
  parseUrlPiece = verifyStrikeSortOrderEither

instance Show StrikeSortOrder where
  show StrikeSortOrderAscend = "ascend"
  show StrikeSortOrderAscendGuessesCount = "ascend_guesses_count"
  show StrikeSortOrderDescend = "descend"
  show StrikeSortOrderDescendGuessesCount = "descend_guesses_count"

sortOrderFromStrikeSortOrder :: StrikeSortOrder -> SortOrder
sortOrderFromStrikeSortOrder StrikeSortOrderAscend = Ascend
sortOrderFromStrikeSortOrder StrikeSortOrderAscendGuessesCount = Ascend
sortOrderFromStrikeSortOrder StrikeSortOrderDescend = Descend
sortOrderFromStrikeSortOrder StrikeSortOrderDescendGuessesCount = Descend

verifyStrikeSortOrderEither :: Text -> Either Text StrikeSortOrder
verifyStrikeSortOrderEither t =
  case found of
    [] -> Left "verifyStrikeSortOrderEither: wrong value"
    ((v, _):_) -> Right v
  where
    found = List.dropWhile (\(_, v)-> v /= t) $ List.map (\v -> (v, Text.pack (show v))) $ enumFrom minBound

verifyStrikeSortOrder :: Text-> StrikeSortOrder
verifyStrikeSortOrder v =
  case verifyStrikeSortOrderEither v of
    Right some -> some
    Left some -> error $ Text.unpack some
