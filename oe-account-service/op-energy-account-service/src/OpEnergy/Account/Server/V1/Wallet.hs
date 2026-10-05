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
import           OpEnergy.Account.Server.V1.Wallet.Mock (mockWalletBackend)

-- | builds the wallet backend named by WALLET_BACKEND.
--
-- The lnbits backend is not implemented yet: it arrives with the node and
-- the LNBits instance, and only this module and its own module change then.
-- Until that, a service configured to use it refuses to start rather than
-- failing on the first request
newWalletBackend
  :: MonadIO m
  => Config
  -> Pool SqlBackend
  -> m WalletBackend
newWalletBackend config pool = case configWalletBackend config of
  WalletModeMock -> return $! mockWalletBackend pool
  WalletModeLNBits -> error
    $ "newWalletBackend: WALLET_BACKEND is lnbits, which needs the LNBits "
    <> "backend. It is not implemented yet: set WALLET_BACKEND to mock"
