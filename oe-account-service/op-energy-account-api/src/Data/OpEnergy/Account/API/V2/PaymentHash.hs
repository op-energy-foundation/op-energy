{-- | Identifier of a lightning payment: the hash of its preimage, as 64
 - hexadecimal characters.
 -}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Data.OpEnergy.Account.API.V2.PaymentHash
  ( PaymentHash(..)
  , defaultPaymentHash
  , everifyPaymentHash
  ) where

import           Data.Aeson
import           Data.Text                  (Text)
import qualified Data.Text as Text
import           Data.Typeable              (Typeable)
import           GHC.Generics
import           Control.Lens               ((&), (?~))
import           Data.Char                  (isHexDigit, toLower)
import           Data.Swagger

-- | hash of a lightning payment's preimage
newtype PaymentHash = PaymentHash
  { unPaymentHash :: Text
  }
  deriving (Show, Eq, Ord, Generic, Typeable)

defaultPaymentHash :: PaymentHash
defaultPaymentHash = PaymentHash
  "9d1a1f0f4e1b47ef8b7a8e5c1f7d3a21b9c84f06d2e3a5178c0b4d9e6f2a1c35"

-- | checks the shape of a payment hash. 'FromJSON' goes through it, so an
-- unchecked value can not enter the service through the API.
--
-- A caller, which has a hash it made itself, decides what an unexpected
-- shape means to it: there is no verifier here, which calls 'error' on its
-- behalf
everifyPaymentHash :: Text -> Either Text PaymentHash
everifyPaymentHash raw
  | Text.length lowered /= 64 =
      Left "PaymentHash: expected 64 characters"
  | not (Text.all isHexDigit lowered) =
      Left "PaymentHash: expected hexadecimal characters only"
  | otherwise = Right (PaymentHash lowered)
  where
    lowered = Text.map toLower (Text.strip raw)

instance ToJSON PaymentHash where
  toJSON = toJSON . unPaymentHash
instance FromJSON PaymentHash where
  parseJSON = withText "PaymentHash"
    $ either (fail . Text.unpack) pure . everifyPaymentHash
instance ToSchema PaymentHash where
  declareNamedSchema _ = pure $ NamedSchema (Just "PaymentHash") $ mempty
    & type_ ?~ SwaggerString
    & description ?~ "hash of a lightning payment's preimage, 64 hex characters"
    & example ?~ toJSON defaultPaymentHash
