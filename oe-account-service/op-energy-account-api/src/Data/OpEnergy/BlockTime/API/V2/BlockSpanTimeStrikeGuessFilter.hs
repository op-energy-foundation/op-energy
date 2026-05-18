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
module Data.OpEnergy.BlockTime.API.V2.BlockSpanTimeStrikeGuessFilter
  ( BlockSpanTimeStrikeGuessFilter(..)
  , sortOrderFromStrikeSortOrder
  , verifyStrikeSortOrderEither
  , verifyStrikeSortOrder
  ) where


import           Data.Swagger
import           Control.Lens
import           GHC.Generics
import           Data.Aeson as Aeson
import           Data.Text ( Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString.Lazy as BS
import           Data.Time.Clock.POSIX(POSIXTime)
import qualified Data.List as List

import           Data.Default
import           Data.Proxy
import           Servant.API(ToHttpApiData(..), FromHttpApiData(..))
import           Database.Persist.Pagination(SortOrder(..))

import qualified Data.OpEnergy.Account.API.V1.BlockTimeStrikeFilterClass
                 as V1
import           Data.OpEnergy.API.V1.Block(BlockHash, BlockHeight)
import qualified Data.OpEnergy.API.V1.Hash as BlockHash (defaultHash)
import           Data.OpEnergy.API.V1.Positive(Positive)
import           Data.OpEnergy.Account.API.V1.FilterRequest()
import           Data.OpEnergy.Account.API.V1.SlowFast


data BlockSpanTimeStrikeGuessFilter = BlockSpanTimeStrikeGuessFilter
  { strikeMediantimeGTE        :: Maybe POSIXTime
  , strikeMediantimeLTE        :: Maybe POSIXTime
  , strikeMediantimeEQ         :: Maybe POSIXTime
  , strikeMediantimeNEQ        :: Maybe POSIXTime
  , strikeBlockHeightGTE       :: Maybe BlockHeight
  , strikeBlockHeightLTE       :: Maybe BlockHeight
  , strikeBlockHeightEQ        :: Maybe BlockHeight
  , strikeBlockHeightNEQ       :: Maybe BlockHeight
  , observedBlockHashEQ        :: Maybe BlockHash
  , observedBlockHashNEQ       :: Maybe BlockHash
  , observedResultEQ           :: Maybe SlowFast
  , observedResultNEQ          :: Maybe SlowFast
  , guessEQ                    :: Maybe SlowFast
  , guessNEQ                   :: Maybe SlowFast
  , sort                       :: Maybe StrikeSortOrder
  , _class                     :: Maybe V1.BlockTimeStrikeFilterClass
  , linesPerPage               :: Maybe (Positive Int)
  }
  deriving (Eq, Show, Generic)
instance ToSchema BlockSpanTimeStrikeGuessFilter where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ Text.unlines
      [ "This is the example of the public filter available for "
      , "BlockSpanTimeStrikeGuess"
      ]
    & mapped.schema.example ?~ toJSON defaultBlockSpanTimeStrikeGuessFilter
instance ToParamSchema BlockSpanTimeStrikeGuessFilter where
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
instance ToJSON BlockSpanTimeStrikeGuessFilter
instance FromJSON BlockSpanTimeStrikeGuessFilter
instance Default BlockSpanTimeStrikeGuessFilter where
  def = defaultBlockSpanTimeStrikeGuessFilter

defaultBlockSpanTimeStrikeGuessFilter :: BlockSpanTimeStrikeGuessFilter
defaultBlockSpanTimeStrikeGuessFilter = BlockSpanTimeStrikeGuessFilter
  { strikeMediantimeGTE        = Just 1
  , strikeMediantimeLTE        = Just 1
  , strikeMediantimeEQ         = Just 2
  , strikeMediantimeNEQ        = Just 2
  , strikeBlockHeightGTE       = Just 3
  , strikeBlockHeightLTE       = Just 3
  , strikeBlockHeightEQ        = Just 3
  , strikeBlockHeightNEQ       = Just 3
  , observedBlockHashEQ        = Just BlockHash.defaultHash
  , observedBlockHashNEQ       = Just BlockHash.defaultHash
  , observedResultEQ           = Just Slow
  , observedResultNEQ          = Just Fast
  , guessEQ                    = Just Slow
  , guessNEQ                   = Just Fast
  , sort                       = Just StrikeSortOrderDescend
  , _class                     = Just V1.BlockTimeStrikeFilterClassGuessable
  , linesPerPage               = Just 100
  }

-- | we need to use separate sort order as we can sort strikes by guesses count
data StrikeSortOrder
  = StrikeSortOrderAscend
  | StrikeSortOrderDescend
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
    & enum_ ?~ (map toJSON $ enumFrom StrikeSortOrderAscend)
instance ToHttpApiData StrikeSortOrder where
  toUrlPiece = Text.pack . show
instance FromHttpApiData StrikeSortOrder where
  parseUrlPiece = verifyStrikeSortOrderEither

instance Show StrikeSortOrder where
  show StrikeSortOrderAscend = "ascend"
  show StrikeSortOrderDescend = "descend"

sortOrderFromStrikeSortOrder :: StrikeSortOrder -> SortOrder
sortOrderFromStrikeSortOrder StrikeSortOrderAscend = Ascend
sortOrderFromStrikeSortOrder StrikeSortOrderDescend = Descend

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

