{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Data.OpEnergy.Account.API.V1.Sats
  ( Sats(..)
  , defaultSats
  ) where

import           Data.Swagger
import           Control.Lens
import           GHC.Generics
import           Data.Typeable              (Typeable)
import           Data.Aeson
import           Data.Word                  (Word64)

-- | balance or transfer amount in satoshis, with type safety so the
-- compiler catches misuse (e.g. passing a block height where sats are
-- expected)
newtype Sats = Sats { unSats :: Word64 }
  deriving (Show, Eq, Ord, Generic, Typeable, Num)

defaultSats :: Sats
defaultSats = Sats 300000

instance ToJSON Sats where
  toJSON (Sats s) = toJSON s
instance FromJSON Sats where
  parseJSON v = Sats <$> parseJSON v
instance ToSchema Sats where
  declareNamedSchema _ = pure $ NamedSchema (Just "Sats") $ mempty
    & type_ ?~ SwaggerInteger
    & example ?~ toJSON defaultSats
