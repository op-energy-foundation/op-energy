{-- | What a change of a balance relates to: a record of the service, which
 - asked for it, named as @kind:id@.
 -}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Data.OpEnergy.Account.API.V2.LedgerReference
  ( LedgerReference(..)
  , defaultLedgerReference
  , maxReferenceLength
  , everifyLedgerReference
  ) where

import           Data.Aeson
import           Data.Text                  (Text)
import qualified Data.Text as Text
import           Data.Typeable              (Typeable)
import           GHC.Generics
import           Control.Lens               ((&), (?~))
import           Data.Swagger

-- | the record a change of a balance relates to, eg "offer:42"
newtype LedgerReference = LedgerReference
  { unLedgerReference :: Text
  }
  deriving (Show, Eq, Ord, Generic, Typeable)

defaultLedgerReference :: LedgerReference
defaultLedgerReference = LedgerReference "offer:42"

-- | the longest reference a caller may send. A reference names a record of
-- the calling service, eg "offer:42", so this is far above anything, which
-- describes one: the column is unbounded and the whole body is read into
-- memory before a handler sees it, so without a bound one call stores as
-- much as it is sent
maxReferenceLength :: Int
maxReferenceLength = 128

-- | checks the length of a reference. 'FromJSON' goes through it, so an
-- unchecked reference can not enter the service through the API
everifyLedgerReference :: Text -> Either Text LedgerReference
everifyLedgerReference raw
  | Text.length raw > maxReferenceLength =
      Left "everifyLedgerReference: reference is longer than allowed"
  | otherwise = Right (LedgerReference raw)

instance ToJSON LedgerReference where
  toJSON = toJSON . unLedgerReference
instance FromJSON LedgerReference where
  parseJSON = withText "LedgerReference"
    $ either (fail . Text.unpack) pure . everifyLedgerReference
instance ToSchema LedgerReference where
  declareNamedSchema _ = pure
    $ NamedSchema (Just "LedgerReference") $ mempty
    & type_ ?~ SwaggerString
    & description ?~
      ( "the record a change of a balance relates to, eg \"offer:42\", at \
        \most " <> Text.pack (show maxReferenceLength) <> " characters"
      )
    & example ?~ toJSON defaultLedgerReference
