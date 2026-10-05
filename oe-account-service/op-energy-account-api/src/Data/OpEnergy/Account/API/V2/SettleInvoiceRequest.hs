{-- | Body of the mock wallet's settle call: which invoice to mark paid.
 -}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE OverloadedStrings          #-}
module Data.OpEnergy.Account.API.V2.SettleInvoiceRequest
  ( SettleInvoiceRequest(..)
  , defaultSettleInvoiceRequest
  ) where

import           Data.Swagger
import           Control.Lens
import           GHC.Generics
import           Data.Typeable              (Typeable)
import           Data.Aeson

import           Data.OpEnergy.Account.API.V2.PaymentHash
                 ( PaymentHash(..), defaultPaymentHash
                 )

-- | request to mark an invoice paid, as a payer would
data SettleInvoiceRequest = SettleInvoiceRequest
  { paymentHash :: PaymentHash
  }
  deriving (Show, Generic, Typeable)

defaultSettleInvoiceRequest :: SettleInvoiceRequest
defaultSettleInvoiceRequest = SettleInvoiceRequest defaultPaymentHash

instance ToJSON SettleInvoiceRequest
instance FromJSON SettleInvoiceRequest
instance ToSchema SettleInvoiceRequest where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "SettleInvoiceRequest schema"
    & mapped.schema.example ?~ toJSON defaultSettleInvoiceRequest
