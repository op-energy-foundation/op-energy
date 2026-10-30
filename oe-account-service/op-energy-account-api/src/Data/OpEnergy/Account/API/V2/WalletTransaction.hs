{-- | One line of an account's wallet history.
 -
 - Fields are prefixed, as a record field named "id" would clash with
 - Prelude's id and "type" is a keyword: the JSON names are produced by
 - dropping the prefix, as PagingResult does.
 -}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE OverloadedStrings          #-}
module Data.OpEnergy.Account.API.V2.WalletTransaction
  ( WalletTransaction(..)
  , defaultWalletTransaction
  ) where

import           Data.Swagger
import qualified Data.Swagger as S
import           Control.Lens
import           GHC.Generics
import           Data.Aeson as A
import qualified Data.Char as Char
import           Data.Default
import qualified Data.List as List
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Int (Int64)
import           Data.Typeable (Typeable)
import           Data.Word (Word64)

import           Data.OpEnergy.Account.API.V1.Sats (Sats(..))
import           Data.OpEnergy.Account.API.V2.LedgerReason
                 ( LedgerReason(..), defaultLedgerReason
                 )

-- | one change of an account's balance, as its owner sees it
data WalletTransaction = WalletTransaction
  { walletTransactionId :: Text
  , walletTransactionAmountSats :: Int64
    -- ^ signed: negative when the balance went down. Sats itself is
    -- unsigned, as a balance can not be negative, so a change carries its
    -- own sign here
  , walletTransactionReason :: LedgerReason
  , walletTransactionBalanceAfter :: Sats
  , walletTransactionReference :: Maybe Text
    -- ^ what the change relates to, eg "offer:42"
  , walletTransactionCreatedAt :: Word64
    -- ^ unix time in seconds
  }
  deriving (Show, Generic, Typeable)

defaultWalletTransaction :: WalletTransaction
defaultWalletTransaction = WalletTransaction
  { walletTransactionId = "1"
  , walletTransactionAmountSats = -5000
  , walletTransactionReason = defaultLedgerReason
  , walletTransactionBalanceAfter = Sats 295000
  , walletTransactionReference = Just "offer:42"
  , walletTransactionCreatedAt = 1790000000
  }

instance Default WalletTransaction where
  def = defaultWalletTransaction

-- | drops the "walletTransaction" prefix of every field name
walletTransactionFieldLabel :: String -> String
walletTransactionFieldLabel =
  ( \s -> case s of
      [] -> []
      (h:t) -> Char.toLower h : t
  ) . List.drop (Text.length "walletTransaction")

instance ToJSON WalletTransaction where
  toJSON = genericToJSON defaultOptions
    { A.fieldLabelModifier = walletTransactionFieldLabel }
  toEncoding = genericToEncoding defaultOptions
    { A.fieldLabelModifier = walletTransactionFieldLabel }
instance FromJSON WalletTransaction where
  parseJSON = genericParseJSON defaultOptions
    { A.fieldLabelModifier = walletTransactionFieldLabel }
instance ToSchema WalletTransaction where
  declareNamedSchema proxy = genericDeclareNamedSchema
    defaultSchemaOptions
      { S.fieldLabelModifier = walletTransactionFieldLabel }
    proxy
    & mapped.schema.description ?~ "WalletTransaction schema"
    & mapped.schema.example ?~ toJSON defaultWalletTransaction
