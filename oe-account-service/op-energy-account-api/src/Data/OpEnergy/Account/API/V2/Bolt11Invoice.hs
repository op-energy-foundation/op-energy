{-- | A lightning payment request, as the bolt11 string a wallet shows as
 - text and as a QR code.
 -}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Data.OpEnergy.Account.API.V2.Bolt11Invoice
  ( Bolt11Invoice(..)
  , defaultBolt11Invoice
  , verifyBolt11Invoice
  , everifyBolt11Invoice
  ) where

import           Data.Aeson
import           Data.Text                  (Text)
import qualified Data.Text as Text
import           Data.Typeable              (Typeable)
import           GHC.Generics
import           Control.Lens               ((&), (?~))
import           Data.Char                  (isAlphaNum, toLower)
import           Data.Swagger
import           Servant.API                (FromHttpApiData(..), ToHttpApiData(..))

-- | a lightning payment request
newtype Bolt11Invoice = Bolt11Invoice
  { unBolt11Invoice :: Text
  }
  deriving (Show, Eq, Ord, Generic, Typeable)

defaultBolt11Invoice :: Bolt11Invoice
defaultBolt11Invoice = Bolt11Invoice "lntbs1u1pnmockinvoice0example"

-- | shortest and longest invoice accepted, so neither an empty string nor an
-- unbounded one reaches the wallet backend
minInvoiceLength, maxInvoiceLength :: Int
minInvoiceLength = 20
maxInvoiceLength = 2048

-- | checks the shape of an invoice. 'FromJSON' and 'FromHttpApiData' both
-- go through it, so an unchecked value can not enter the service through the
-- API. The shape is all it checks: what the invoice says is the wallet
-- backend's answer, never the client's
everifyBolt11Invoice :: Text -> Either Text Bolt11Invoice
everifyBolt11Invoice raw
  | Text.length cleaned < minInvoiceLength =
      Left "Bolt11Invoice: too short to be an invoice"
  | Text.length cleaned > maxInvoiceLength =
      Left "Bolt11Invoice: too long to be an invoice"
  | not ("ln" `Text.isPrefixOf` cleaned) =
      Left "Bolt11Invoice: expected an invoice starting with ln"
  | not (Text.all isAlphaNum cleaned) =
      Left "Bolt11Invoice: expected letters and digits only"
  | otherwise = Right (Bolt11Invoice cleaned)
  where
    -- wallets copy invoices with a "lightning:" scheme and in upper case
    cleaned = maybe lowered id (Text.stripPrefix "lightning:" lowered)
    lowered = Text.map toLower (Text.strip raw)

-- | 'everifyBolt11Invoice', which fails instead of returning an error
verifyBolt11Invoice :: Text -> Bolt11Invoice
verifyBolt11Invoice raw =
  either (error . Text.unpack) id (everifyBolt11Invoice raw)

instance ToJSON Bolt11Invoice where
  toJSON = toJSON . unBolt11Invoice
instance FromJSON Bolt11Invoice where
  parseJSON = withText "Bolt11Invoice"
    $ either (fail . Text.unpack) pure . everifyBolt11Invoice
instance ToSchema Bolt11Invoice where
  declareNamedSchema _ = pure $ NamedSchema (Just "Bolt11Invoice") $ mempty
    & type_ ?~ SwaggerString
    & description ?~ "lightning payment request (bolt11)"
    & example ?~ toJSON defaultBolt11Invoice
instance ToParamSchema Bolt11Invoice where
  toParamSchema _ = mempty
    & type_ ?~ SwaggerString
instance FromHttpApiData Bolt11Invoice where
  parseQueryParam = everifyBolt11Invoice
instance ToHttpApiData Bolt11Invoice where
  toQueryParam = unBolt11Invoice
