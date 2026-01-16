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
module Data.OpEnergy.BlockTime.API.V2.BlockSpanTimeStrikeGuessesSummary
  ( BlockSpanTimeStrikeGuessesSummary(..)
  ) where

import           Data.Swagger
import           Control.Lens
import           GHC.Generics
import           Data.Aeson as Aeson
import qualified Data.Text as Text

import           Data.Proxy
import           Data.Default

import           Data.OpEnergy.API.V1.Natural
import           Data.OpEnergy.Account.API.V1.FilterRequest()
import           Data.OpEnergy.Account.API.V1.Common


-- | this data type defines data structure, that will be used in API to
-- represent BlockSpanTimeStrike with possible observed result, judgement
-- block's data, span size and possible blockspan headers with nbdr and hashrate
data BlockSpanTimeStrikeGuessesSummary = BlockSpanTimeStrikeGuessesSummary
  { slowCount :: Natural Int
  , fastCount :: Natural Int
  }
  deriving (Show, Generic)
instance FromJSON BlockSpanTimeStrikeGuessesSummary
instance ToJSON   BlockSpanTimeStrikeGuessesSummary
instance ToSchema BlockSpanTimeStrikeGuessesSummary where
  declareNamedSchema proxy = genericDeclareNamedSchema (commonSchemaOptions (def1 proxy)) proxy
    & mapped.schema.type_ ?~ SwaggerObject
    & mapped.schema.description ?~ Text.unlines
        [ "defines BlockSpanTimeStrikeGuessesSummary data structure. where:"
        , "- slowCount - 'slow' guesses count"
        , "- fastCount - 'fast guesses count"
        ]
    & mapped.schema.example ?~ toJSON defaultBlockSpanTimeStrikeGuessesSummary
    where
      def1 :: Default a => Proxy a -> a
      def1 _ = def
instance Default BlockSpanTimeStrikeGuessesSummary where
  def = defaultBlockSpanTimeStrikeGuessesSummary
defaultBlockSpanTimeStrikeGuessesSummary :: BlockSpanTimeStrikeGuessesSummary
defaultBlockSpanTimeStrikeGuessesSummary =  BlockSpanTimeStrikeGuessesSummary
  { slowCount = verifyNatural 0
  , fastCount = verifyNatural 0
  }

