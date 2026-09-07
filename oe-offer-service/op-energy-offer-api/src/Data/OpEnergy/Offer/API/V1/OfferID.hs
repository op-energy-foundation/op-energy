{-- | Typed wrapper for offer identifiers.
 -
 - Extracted to its own module to avoid import cycles between
 - 'OfferInfo' and 'ContractInfo'.
 -}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE OverloadedStrings          #-}
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
import           Servant.API                (FromHttpApiData(..), ToHttpApiData(..))

-- | typed wrapper for offer identifiers
newtype OfferID = OfferID { unOfferID :: Text }
  deriving (Show, Eq, Generic, Typeable)
instance ToJSON OfferID where
  toJSON (OfferID t) = toJSON t
instance FromJSON OfferID where
  parseJSON = withText "OfferID" $ pure . OfferID
instance ToSchema OfferID where
  declareNamedSchema _ = pure $ NamedSchema (Just "OfferID") $ mempty
    & type_ ?~ SwaggerString
    & example ?~ toJSON defaultOfferID
instance ToParamSchema OfferID where
  toParamSchema _ = mempty
    & type_ ?~ SwaggerString
instance FromHttpApiData OfferID where
  parseQueryParam = Right . OfferID
instance ToHttpApiData OfferID where
  toQueryParam (OfferID t) = t

defaultOfferID :: OfferID
defaultOfferID = OfferID "1"
