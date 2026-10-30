{-- | Which wallet the service talks to, as reported to clients, so the
 - frontend can tell a mocked wallet from a real one.
 -}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE OverloadedStrings          #-}
module Data.OpEnergy.Account.API.V2.WalletMode
  ( WalletMode(..)
  , defaultWalletMode
  , walletModeToText
  , walletModeFromText
  ) where

import           Data.Aeson
import           Data.Text                  (Text)
import qualified Data.Text as Text
import           Data.Typeable              (Typeable)
import           GHC.Generics
import           Control.Lens               ((&), (?~))
import           Data.Default
import           Data.Swagger

-- | the wallets this service can talk to
data WalletMode
  = WalletModeMock
    -- ^ invoices and payments are kept in this service's own database, so
    -- the wallet works before a lightning node exists. No real sats move
  | WalletModeLNBits
    -- ^ invoices and payments go to an LNBits instance in front of a node
  deriving (Show, Eq, Ord, Generic, Typeable, Enum, Bounded)

walletModeToText :: WalletMode -> Text
walletModeToText WalletModeMock   = "mock"
walletModeToText WalletModeLNBits = "lnbits"

walletModeFromText :: Text -> Either Text WalletMode
walletModeFromText "mock"   = Right WalletModeMock
walletModeFromText "lnbits" = Right WalletModeLNBits
walletModeFromText other    = Left $ "WalletMode: unknown wallet: " <> other

defaultWalletMode :: WalletMode
defaultWalletMode = WalletModeMock

instance Default WalletMode where
  def = defaultWalletMode
instance ToJSON WalletMode where
  toJSON = toJSON . walletModeToText
instance FromJSON WalletMode where
  parseJSON = withText "WalletMode"
    $ either (fail . Text.unpack) pure . walletModeFromText
instance ToSchema WalletMode where
  declareNamedSchema _ = pure $ NamedSchema (Just "WalletMode") $ mempty
    & type_ ?~ SwaggerString
    & enum_ ?~ map (toJSON . walletModeToText) [minBound .. maxBound]
    & example ?~ toJSON defaultWalletMode
