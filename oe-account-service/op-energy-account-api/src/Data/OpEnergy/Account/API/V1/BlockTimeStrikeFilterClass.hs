{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE DerivingStrategies         #-}
module Data.OpEnergy.Account.API.V1.BlockTimeStrikeFilterClass where

import           Data.Swagger
import           Control.Lens
import           GHC.Generics
import           Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import           Servant.API(ToHttpApiData(..), FromHttpApiData(..))
import           Data.Default

import           Data.OpEnergy.Account.API.V1.Common

data BlockTimeStrikeFilterClass
  = BlockTimeStrikeFilterClassGuessable
  | BlockTimeStrikeFilterClassOutcomeKnown
  | BlockTimeStrikeFilterClassOutcomeUnknown
  deriving (Eq, Enum, Generic)
instance Show BlockTimeStrikeFilterClass where
  show BlockTimeStrikeFilterClassGuessable = "BlockTimeStrikeFilterClass Guessable"
  show BlockTimeStrikeFilterClassOutcomeKnown = "BlockTimeStrikeFilterClass OutcomeKnown"
  show BlockTimeStrikeFilterClassOutcomeUnknown = "BlockTimeStrikeFilterClass OutcomeUnknown"
instance ToSchema BlockTimeStrikeFilterClass where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ (Text.unlines
      [ "This is the examples BlockTimeStrikeFilterClass values"
      ])
    & mapped.schema.example ?~ toJSON (map toJSON $ enumFrom BlockTimeStrikeFilterClassGuessable)
instance ToParamSchema BlockTimeStrikeFilterClass where
  toParamSchema _ = mempty
    & type_ ?~ SwaggerString
    & enum_ ?~ (map toJSON $ enumFrom BlockTimeStrikeFilterClassGuessable)
instance ToHttpApiData BlockTimeStrikeFilterClass where
  toUrlPiece v =
    case toJSON v of
      String ret -> ret
      some -> error ("ToHttpApiData BlockTimeStrikeFilterClass: " ++ show some)
instance FromHttpApiData BlockTimeStrikeFilterClass where
  parseUrlPiece v =
    case eitherDecodeStrict (Text.encodeUtf8 v) of
      Left some -> Left (Text.pack some)
      Right some -> Right some
instance ToJSON BlockTimeStrikeFilterClass where
  toJSON = commonToJSON genericToJSON
  toEncoding = commonToJSON genericToEncoding
instance FromJSON BlockTimeStrikeFilterClass where
  parseJSON = commonParseJSON
instance Default BlockTimeStrikeFilterClass where
  def = defaultBlockTimeStrikeFilterClass

defaultBlockTimeStrikeFilterClass :: BlockTimeStrikeFilterClass
defaultBlockTimeStrikeFilterClass =  BlockTimeStrikeFilterClassGuessable

