{-- | The invoice a client shows as text and as a QR code.
 -}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE OverloadedStrings          #-}
module Data.OpEnergy.Account.API.V2.CreateInvoiceResult
  ( CreateInvoiceResult(..)
  , defaultCreateInvoiceResult
  ) where

import           Data.Swagger
import           Control.Lens
import           GHC.Generics
import           Data.Typeable              (Typeable)
import           Data.Word                  (Word64)
import           Data.Aeson

import           Data.OpEnergy.Account.API.V1.Sats (Sats(..))
import           Data.OpEnergy.Account.API.V2.Bolt11Invoice
                 ( Bolt11Invoice(..), defaultBolt11Invoice
                 )
import           Data.OpEnergy.Account.API.V2.PaymentHash
                 ( PaymentHash(..), defaultPaymentHash
                 )

-- | an invoice, which has just been created
data CreateInvoiceResult = CreateInvoiceResult
  { invoice :: Bolt11Invoice
  , paymentHash :: PaymentHash
    -- ^ identifies this payment, eg when asking whether it has been paid
  , amountSats :: Sats
  , expiresAt :: Word64
    -- ^ unix time in seconds, after which the invoice can not be paid
  }
  deriving (Show, Generic, Typeable)

defaultCreateInvoiceResult :: CreateInvoiceResult
defaultCreateInvoiceResult = CreateInvoiceResult
  defaultBolt11Invoice defaultPaymentHash (Sats 50000) 1790000000

instance ToJSON CreateInvoiceResult
instance FromJSON CreateInvoiceResult
instance ToSchema CreateInvoiceResult where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "CreateInvoiceResult schema"
    & mapped.schema.example ?~ toJSON defaultCreateInvoiceResult
