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
module Data.OpEnergy.Account.API.V1.SlowFast
  ( SlowFast(..)
  ) where

import           GHC.Generics
import           Data.Text(Text)
import qualified Data.Text as Text

import           Data.Aeson as Aeson
import           Servant.API(ToHttpApiData(..), FromHttpApiData(..))
import           Data.Default
import           Control.Lens
import           Data.Swagger

data SlowFast
  = Slow
  | Fast
  deriving (Eq, Enum, Show, Bounded, Ord, Generic)

instance ToJSON SlowFast where
  toJSON Slow = toJSON ("slow" :: Text)
  toJSON Fast = toJSON ("fast" :: Text)
instance FromJSON SlowFast where
  parseJSON = withText "SlowFast" $! pure . verifySlowFast

instance ToSchema SlowFast where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~
      Text.unlines
      [ ""
      ]
    & mapped.schema.example ?~ toJSON (map toJSON $ enumFrom Slow)
instance ToParamSchema SlowFast where
  toParamSchema _ = mempty
    & type_ ?~ SwaggerString
    & enum_ ?~ (map toJSON $ enumFrom Slow)
instance ToHttpApiData SlowFast where
  toUrlPiece Slow = "slow"
  toUrlPiece Fast = "fast"
instance FromHttpApiData SlowFast where
  parseUrlPiece "slow" = Prelude.Right Slow
  parseUrlPiece "fast" = Prelude.Right Fast
  parseUrlPiece _ = Left "wrong SlowFast value"
instance Default SlowFast where
  def = defaultSlowFast

defaultSlowFast :: SlowFast
defaultSlowFast = Slow

verifySlowFastEither :: Text -> Either Text SlowFast
verifySlowFastEither "slow" = Right Slow
verifySlowFastEither "fast" = Right Fast
verifySlowFastEither v = Left $! "verifySlowFastEither: wrong value: " <> v

verifySlowFast :: Text-> SlowFast
verifySlowFast v = case verifySlowFastEither v of
  Right ret -> ret
  Left reason -> error $! Text.unpack reason


