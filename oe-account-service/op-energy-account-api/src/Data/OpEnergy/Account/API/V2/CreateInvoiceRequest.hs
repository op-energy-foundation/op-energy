{-- | Body of the wallet's create-invoice call: how much an account wants to
 - be paid.
 -}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE OverloadedStrings          #-}
module Data.OpEnergy.Account.API.V2.CreateInvoiceRequest
  ( CreateInvoiceRequest(..)
  , defaultCreateInvoiceRequest
  , maxNoteLength
  , everifyNote
  ) where

import           Data.Swagger
import           Control.Lens
import           GHC.Generics
import           Data.Text                  (Text)
import qualified Data.Text as Text
import           Data.Typeable              (Typeable)
import           Data.Aeson

import           Data.OpEnergy.Account.API.V1.Sats (Sats(..))

-- | request of an invoice to be paid to the authenticated account
data CreateInvoiceRequest = CreateInvoiceRequest
  { amountSats :: Sats
    -- ^ checked against the wallet's smallest and largest allowed amount
  , note :: Maybe Text
    -- ^ recorded with the payment. Nothing reads it back: no response
    -- carries it, so it is stored and not shown anywhere
  }
  deriving (Show, Generic, Typeable)

defaultCreateInvoiceRequest :: CreateInvoiceRequest
defaultCreateInvoiceRequest = CreateInvoiceRequest (Sats 50000) Nothing

-- | the longest note an invoice may carry. The column is unbounded and the
-- whole body is read into memory before a handler sees it, so without a
-- bound one call stores as much as it is sent
maxNoteLength :: Int
maxNoteLength = 500

-- | checks the length of a note, as deserialisation is where a value is
-- checked
everifyNote :: Maybe Text -> Either Text (Maybe Text)
everifyNote Nothing = Right Nothing
everifyNote (Just raw)
  | Text.length raw > maxNoteLength =
      Left "CreateInvoiceRequest: note is longer than allowed"
  | otherwise = Right (Just raw)

instance ToJSON CreateInvoiceRequest
instance FromJSON CreateInvoiceRequest where
  parseJSON = withObject "CreateInvoiceRequest" $ \v-> do
    amountSatsV <- v .: "amountSats"
    noteV <- v .:? "note"
    either (fail . Text.unpack) (return . CreateInvoiceRequest amountSatsV)
      $ everifyNote noteV
instance ToSchema CreateInvoiceRequest where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "CreateInvoiceRequest schema"
    & mapped.schema.example ?~ toJSON defaultCreateInvoiceRequest
