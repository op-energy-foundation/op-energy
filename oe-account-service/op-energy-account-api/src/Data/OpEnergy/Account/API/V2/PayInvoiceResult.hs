{-- | What a payment cost and what it left in the account.
 -}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE OverloadedStrings          #-}
module Data.OpEnergy.Account.API.V2.PayInvoiceResult
  ( PayInvoiceResult(..)
  , defaultPayInvoiceResult
  ) where

import           Data.Swagger
import           Control.Lens
import           GHC.Generics
import           Data.Typeable              (Typeable)
import           Data.Aeson

import           Data.OpEnergy.Account.API.V1.Sats (Sats(..))
import           Data.OpEnergy.Account.API.V2.PaymentHash
                 ( PaymentHash(..), defaultPaymentHash
                 )

-- | a payment, which has been made
data PayInvoiceResult = PayInvoiceResult
  { paymentHash :: PaymentHash
  , amountSats :: Sats
  , feeSats :: Sats
  , balanceSats :: Sats
    -- ^ balance after the payment, so a client needs no second call
  }
  deriving (Show, Generic, Typeable)

defaultPayInvoiceResult :: PayInvoiceResult
defaultPayInvoiceResult = PayInvoiceResult
  defaultPaymentHash (Sats 50000) (Sats 0) (Sats 250000)

instance ToJSON PayInvoiceResult
instance FromJSON PayInvoiceResult
instance ToSchema PayInvoiceResult where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "PayInvoiceResult schema"
    & mapped.schema.example ?~ toJSON defaultPayInvoiceResult
