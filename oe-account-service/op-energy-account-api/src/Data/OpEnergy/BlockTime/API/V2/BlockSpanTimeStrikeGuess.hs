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
module Data.OpEnergy.BlockTime.API.V2.BlockSpanTimeStrikeGuess
  ( BlockSpanTimeStrikeGuess(..)
  ) where

import           Data.Swagger
import           Control.Lens
import           GHC.Generics
import           Data.Aeson as Aeson
import qualified Data.Text as Text
import           Data.Time.Clock.POSIX(POSIXTime)

import           Data.Proxy
import           Data.Default

import           Data.OpEnergy.Account.API.V1.FilterRequest()
import           Data.OpEnergy.Account.API.V1.Common
import           Data.OpEnergy.Account.API.V1.SlowFast
import           Data.OpEnergy.Account.API.V1.Account
import           Data.OpEnergy.Account.API.V1.UUID

import           Data.OpEnergy.BlockTime.API.V2.BlockSpanTimeStrike


-- | this data type defines data structure, that will be used in API to
-- represent BlockSpanTimeStrikeGuess with possible observed result, judgement
-- block's data, span size and possible blockspan headers with nbdr and hashrate
data BlockSpanTimeStrikeGuess = BlockSpanTimeStrikeGuess
  { person :: UUID Person
  , strike ::  BlockSpanTimeStrike
  , creationTime :: POSIXTime
  , guess :: SlowFast
  }
  deriving (Show, Generic)
instance FromJSON BlockSpanTimeStrikeGuess
instance ToJSON BlockSpanTimeStrikeGuess
instance ToSchema BlockSpanTimeStrikeGuess where
  declareNamedSchema proxy = genericDeclareNamedSchema (commonSchemaOptions (def1 proxy)) proxy
    & mapped.schema.type_ ?~ SwaggerObject
    & mapped.schema.description ?~ Text.unlines
        [ "defines BlockSpanTimeStrikeGuess data structure. where:"
        ]
    & mapped.schema.example ?~ toJSON defaultBlockSpanTimeStrikeGuess
    where
      def1 :: Default a => Proxy a -> a
      def1 _ = def
instance Default BlockSpanTimeStrikeGuess where
  def = defaultBlockSpanTimeStrikeGuess
defaultBlockSpanTimeStrikeGuess :: BlockSpanTimeStrikeGuess
defaultBlockSpanTimeStrikeGuess =  BlockSpanTimeStrikeGuess
  { person = defaultUUID
  , strike = def
  , creationTime = defaultPOSIXTime
  , guess = def
  }

