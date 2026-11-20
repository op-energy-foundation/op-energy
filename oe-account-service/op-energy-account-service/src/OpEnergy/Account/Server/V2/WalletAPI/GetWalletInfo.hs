{-- | V2 wallet info handler: the balance of the authenticated account,
 - which wallet is in use and the amounts it accepts.
 -}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE OverloadedStrings          #-}
module OpEnergy.Account.Server.V2.WalletAPI.GetWalletInfo
  ( getWalletInfoHandler
  , getWalletInfo
  ) where

import           Control.Monad.Logger(logError)
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Reader (ask)
import           Database.Persist (Entity(..))

import qualified Data.OpEnergy.Account.API.V1.Account as API
import           Data.OpEnergy.Account.API.V2.WalletInfo
                 ( WalletInfo(..)
                 )

import           OpEnergy.Account.Server.V1.AccountService
                 ( mgetPersonByAccountToken
                 )
import           OpEnergy.Account.Server.V1.Class
                 ( AppM, State(..), profile, runLogging
                 )
import           OpEnergy.Account.Server.V1.Config
                 ( Config(..)
                 )
import           OpEnergy.Account.Server.V1.Person
import           OpEnergy.Account.Server.V1.Wallet.Class
                 ( WalletBackend(..)
                 )
import           OpEnergy.Error
                 ( eitherThrowJSON, runExceptPrefixT
                 , CallstackError, accountNotFound
                 )
import           OpEnergy.ExceptMaybe(exceptTMaybeT)

-- | V2 wallet endpoint
getWalletInfoHandler
  :: API.AccountToken
  -> AppM WalletInfo
getWalletInfoHandler token =
    let name = "V2.getWalletInfoHandler"
    in profile name $ eitherThrowJSON
      ( runLogging . $(logError))
      $ getWalletInfo token

-- | business logic for the V2 wallet endpoint. Reports what the wallet
-- allows, so a client can tell the owner why an amount is refused before it
-- is sent, and whether any real sats are moving
getWalletInfo
  :: API.AccountToken
  -> AppM (Either CallstackError WalletInfo)
getWalletInfo token =
    let name = "V2.getWalletInfo"
    in profile name $ runExceptPrefixT name $ do
  (Entity _ person) <- exceptTMaybeT accountNotFound
    $ mgetPersonByAccountToken token
  State{ config = config, wallet = walletV } <- lift ask
  return $! WalletInfo
    { balanceSats = personBalance person
    , mode = walletKind walletV
    , canSend = walletCanSend walletV
    , canReceive = walletCanReceive walletV
    , minInvoiceSats = configWalletMinInvoiceSats config
    , maxInvoiceSats = configWalletMaxInvoiceSats config
    , maxWithdrawalSats = configWalletMaxWithdrawalSats config
    }
