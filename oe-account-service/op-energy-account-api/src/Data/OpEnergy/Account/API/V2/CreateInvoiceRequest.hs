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
  ) where

import           Data.Swagger
import           Control.Lens
import           GHC.Generics
import           Data.Typeable              (Typeable)
import           Data.Aeson

import           Data.OpEnergy.Account.API.V1.Sats (Sats(..))
import           Data.OpEnergy.Account.API.V2.Note (Note)

-- | request of an invoice to be paid to the authenticated account
data CreateInvoiceRequest = CreateInvoiceRequest
  { amountSats :: Sats
    -- ^ checked against the wallet's smallest and largest allowed amount
  , note :: Maybe Note
    -- ^ recorded with the payment. Nothing reads it back: no response
    -- carries it, so it is stored and not shown anywhere. Its own type
    -- checks it, so this request needs no deserialiser of its own
  }
  deriving (Show, Generic, Typeable)

defaultCreateInvoiceRequest :: CreateInvoiceRequest
defaultCreateInvoiceRequest = CreateInvoiceRequest (Sats 50000) Nothing

instance ToJSON CreateInvoiceRequest
instance FromJSON CreateInvoiceRequest
instance ToSchema CreateInvoiceRequest where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "CreateInvoiceRequest schema"
    & mapped.schema.example ?~ toJSON defaultCreateInvoiceRequest
