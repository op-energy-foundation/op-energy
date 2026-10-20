{-- | This module builds the wallet backend this service talks to, from
 - config, once at startup.
 -
 - It is the only place, which knows that more than one backend exists: a
 - handler asks 'OpEnergy.Account.Server.V1.Class.State' for the wallet and
 - calls it, whichever it is.
 -}
{-# LANGUAGE OverloadedStrings          #-}
module OpEnergy.Account.Server.V1.Wallet
  ( newWalletBackend
  , module OpEnergy.Account.Server.V1.Wallet.Class
  ) where

import           Control.Monad.IO.Class (MonadIO)
import           Data.Pool (Pool)
import           Database.Persist.Sql (SqlBackend)

import           OpEnergy.Account.Server.V1.Config
                 ( Config(..)
                 )
import           Data.OpEnergy.Account.API.V2.WalletMode (WalletMode(..))

import           OpEnergy.Account.Server.V1.Wallet.Class
import           OpEnergy.Account.Server.V1.Wallet.LNBits (lnbitsWalletBackend)
import           OpEnergy.Account.Server.V1.Wallet.Mock (mockWalletBackend)

-- | builds the wallet backend named by WALLET_BACKEND.
--
-- The options the chosen backend needs are checked by
-- 'OpEnergy.Account.Server.V1.Config.everifyWalletConfig' at startup, so a
-- backend is only built here from options, which can work
newWalletBackend
  :: MonadIO m
  => Config
  -> Pool SqlBackend
  -> m WalletBackend
newWalletBackend config pool = case configWalletBackend config of
  WalletModeMock -> return $! mockWalletBackend pool
  WalletModeLNBits -> return $! lnbitsWalletBackend
    (configLNBitsURL config)
    (configLNBitsInvoiceKey config)
    pool
