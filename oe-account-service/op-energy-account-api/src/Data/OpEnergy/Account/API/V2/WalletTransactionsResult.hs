{-- | One page of an account's wallet history, with what is needed to show
 - numbered pages: which page this is, how big a page is and how many
 - movements there are in total.
 -}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE OverloadedStrings          #-}
module Data.OpEnergy.Account.API.V2.WalletTransactionsResult
  ( WalletTransactionsResult(..)
  , defaultWalletTransactionsResult
  ) where

import           Data.Swagger
import           Control.Lens
import           GHC.Generics
import           Data.Typeable              (Typeable)
import           Data.Word                  (Word32, Word64)
import           Data.Aeson

import           Data.OpEnergy.Account.API.V2.WalletTransaction
                 ( WalletTransaction(..), defaultWalletTransaction
                 )

-- | one page of wallet history, newest first
data WalletTransactionsResult = WalletTransactionsResult
  { results :: [ WalletTransaction ]
  , page :: Word32
    -- ^ which page this is, counted from 0
  , pageSize :: Word32
  , totalCount :: Word64
    -- ^ every movement of the account, so a client can show page numbers
  }
  deriving (Show, Generic, Typeable)

defaultWalletTransactionsResult :: WalletTransactionsResult
defaultWalletTransactionsResult = WalletTransactionsResult
  [ defaultWalletTransaction ] 0 100 1

instance ToJSON WalletTransactionsResult
instance FromJSON WalletTransactionsResult
instance ToSchema WalletTransactionsResult where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "WalletTransactionsResult schema"
    & mapped.schema.example ?~ toJSON defaultWalletTransactionsResult
