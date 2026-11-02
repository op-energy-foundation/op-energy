{-- | Body of the wallet's pay call: the invoice an account wants to pay.
 -}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE OverloadedStrings          #-}
module Data.OpEnergy.Account.API.V2.PayInvoiceRequest
  ( PayInvoiceRequest(..)
  , defaultPayInvoiceRequest
  ) where

import           Data.Swagger
import           Control.Lens
import           GHC.Generics
import           Data.Typeable              (Typeable)
import           Data.Aeson

import           Data.OpEnergy.Account.API.V1.Sats (Sats(..))
import           Data.OpEnergy.Account.API.V2.Bolt11Invoice
                 ( Bolt11Invoice(..), defaultBolt11Invoice
                 )

-- | request to pay an invoice from the authenticated account's balance
data PayInvoiceRequest = PayInvoiceRequest
  { invoice :: Bolt11Invoice
  , amountSats :: Maybe Sats
    -- ^ what the client believes the invoice is for. Used only for an
    -- invoice this service did not issue and can not read by itself: an
    -- invoice of this service is always paid for the amount it was created
    -- with, whatever a client asks for
  }
  deriving (Show, Generic, Typeable)

defaultPayInvoiceRequest :: PayInvoiceRequest
defaultPayInvoiceRequest = PayInvoiceRequest defaultBolt11Invoice Nothing

instance ToJSON PayInvoiceRequest
instance FromJSON PayInvoiceRequest
instance ToSchema PayInvoiceRequest where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "PayInvoiceRequest schema"
    & mapped.schema.example ?~ toJSON defaultPayInvoiceRequest
