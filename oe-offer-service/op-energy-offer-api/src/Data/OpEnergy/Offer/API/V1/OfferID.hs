{-- | Typed wrapper for offer identifiers.
 -
 - Extracted to its own module to avoid import cycles between
 - 'OfferInfo' and 'ContractInfo'.
 -}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternGuards              #-}
module Data.OpEnergy.Offer.API.V1.OfferID
  ( OfferID(..)
  , defaultOfferID
  ) where

import           Data.Swagger
import           Control.Lens               ((&), (?~))
import           GHC.Generics
import           Data.Typeable              (Typeable)
import           Data.Aeson
import           Data.Text                  (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import           Servant.API                (FromHttpApiData(..), ToHttpApiData(..))

-- | typed wrapper for offer identifiers
newtype OfferID = OfferID { unOfferID :: Text }
  deriving (Show, Eq, Generic, Typeable)
instance ToJSON OfferID where
  toJSON (OfferID t) = toJSON t
instance FromJSON OfferID where
  parseJSON = withText "OfferID" $ \t ->
    case TR.decimal t of
      Right (n, rest) | T.null rest, (n :: Integer) > 0 -> pure (OfferID t)
      _ -> fail "OfferID must be a positive integer"
instance ToSchema OfferID where
  declareNamedSchema _ = pure $ NamedSchema (Just "OfferID") $ mempty
    & type_ ?~ SwaggerString
    & example ?~ toJSON defaultOfferID
instance ToParamSchema OfferID where
  toParamSchema _ = mempty
    & type_ ?~ SwaggerString
instance FromHttpApiData OfferID where
  parseQueryParam t = case TR.decimal t of
    Right (n, rest) | T.null rest, (n :: Integer) > 0 -> Right (OfferID t)
    _ -> Left "OfferID must be a positive integer"
instance ToHttpApiData OfferID where
  toQueryParam (OfferID t) = t

defaultOfferID :: OfferID
defaultOfferID = OfferID "1"
