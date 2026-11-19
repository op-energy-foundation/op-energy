{-- | Identifier of one line of an account's wallet history, as 32
 - hexadecimal characters.
 -}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Data.OpEnergy.Account.API.V2.WalletTransactionId
  ( WalletTransactionId(..)
  , defaultWalletTransactionId
  , walletTransactionIdLength
  , everifyWalletTransactionId
  ) where

import           Data.Aeson
import           Data.Text                  (Text)
import qualified Data.Text as Text
import           Data.Typeable              (Typeable)
import           GHC.Generics
import           Control.Lens               ((&), (?~))
import           Data.Char                  (isHexDigit, toLower)
import           Data.Swagger

-- | identifier of one movement of an account's balance
newtype WalletTransactionId = WalletTransactionId
  { unWalletTransactionId :: Text
  }
  deriving (Show, Eq, Ord, Generic, Typeable)

defaultWalletTransactionId :: WalletTransactionId
defaultWalletTransactionId =
  WalletTransactionId "4f2c9ab1d8e35760c14b9f0a7e26d3b8"

-- | how long an identifier is: the service names a movement by half of a
-- SHA-256 digest, which is 16 bytes and so twice as many characters. See
-- OpEnergy.Account.Server.V2.WalletAPI.GetTransactions.opaqueId, which is
-- where one is made
walletTransactionIdLength :: Int
walletTransactionIdLength = 32

-- | checks the shape of an identifier. 'FromJSON' goes through it, so an
-- unchecked identifier can not enter the service through the API
everifyWalletTransactionId :: Text -> Either Text WalletTransactionId
everifyWalletTransactionId raw
  | Text.length lowered /= walletTransactionIdLength =
      Left "everifyWalletTransactionId: expected 32 characters"
  | not (Text.all isHexDigit lowered) =
      Left "everifyWalletTransactionId: expected hexadecimal characters only"
  | otherwise = Right (WalletTransactionId lowered)
  where
    lowered = Text.map toLower (Text.strip raw)

instance ToJSON WalletTransactionId where
  toJSON = toJSON . unWalletTransactionId
instance FromJSON WalletTransactionId where
  parseJSON = withText "WalletTransactionId"
    $ either (fail . Text.unpack) pure . everifyWalletTransactionId
instance ToSchema WalletTransactionId where
  declareNamedSchema _ = pure
    $ NamedSchema (Just "WalletTransactionId") $ mempty
    & type_ ?~ SwaggerString
    & description ?~
      ( "identifier of one movement of an account's balance, "
     <> Text.pack (show walletTransactionIdLength) <> " hex characters"
      )
    & example ?~ toJSON defaultWalletTransactionId
