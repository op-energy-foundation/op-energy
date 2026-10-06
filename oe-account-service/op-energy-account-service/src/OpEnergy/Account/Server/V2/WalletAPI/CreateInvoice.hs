{-- | V2 create invoice handler: an invoice, which someone else can pay to
 - the authenticated account.
 -
 - The amount is checked here, against the wallet's configured smallest and
 - largest, so a client can not ask the wallet backend for anything else.
 -}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE OverloadedStrings          #-}
module OpEnergy.Account.Server.V2.WalletAPI.CreateInvoice
  ( createInvoiceHandler
  , createInvoice
  ) where

import           Control.Monad (when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Logger(logError)
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Except (ExceptT(..), throwE)
import           Control.Monad.Trans.Reader (ask)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Database.Persist (Entity(..))

import qualified Data.OpEnergy.Account.API.V1.Account as API
import           Data.OpEnergy.Account.API.V2.CreateInvoiceRequest
                 ( CreateInvoiceRequest(..)
                 )
import           Data.OpEnergy.Account.API.V2.CreateInvoiceResult
                 ( CreateInvoiceResult(..)
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
import           OpEnergy.Account.Server.V1.Wallet.Class
import           OpEnergy.Error
                 ( eitherThrowJSON, runExceptPrefixT
                 , CallstackError, accountNotFound
                 , walletAmountAboveMaximum, walletAmountBelowMinimum
                 )
import           OpEnergy.ExceptMaybe(exceptTMaybeT)

-- | V2 wallet/invoice endpoint
createInvoiceHandler
  :: API.AccountToken
  -> CreateInvoiceRequest
  -> AppM CreateInvoiceResult
createInvoiceHandler token request =
    let name = "V2.createInvoiceHandler"
    in profile name $ eitherThrowJSON
      ( runLogging . $(logError))
      $ createInvoice token request

-- | business logic for the V2 wallet/invoice endpoint
createInvoice
  :: API.AccountToken
  -> CreateInvoiceRequest
  -> AppM (Either CallstackError CreateInvoiceResult)
createInvoice token (CreateInvoiceRequest requestAmountSats requestNote) =
    let name = "V2.createInvoice"
    in profile name $ runExceptPrefixT name $ do
  (Entity key _) <- exceptTMaybeT accountNotFound
    $ mgetPersonByAccountToken token
  State{ config = config, wallet = walletV } <- lift ask
  let amount = requestAmountSats -- named, as it is what the wallet is asked for
  when (amount < configWalletMinInvoiceSats config)
    $ throwE walletAmountBelowMinimum
  when (amount > configWalletMaxInvoiceSats config)
    $ throwE walletAmountAboveMaximum
  now <- liftIO getPOSIXTime
  created <- ExceptT $ liftIO $ walletCreateInvoice walletV $ CreateInvoiceParams
    { createInvoicePersonId = key
    , createInvoiceAmountSats = amount
    , createInvoiceNote = requestNote
    , createInvoiceExpirySecs = configWalletInvoiceExpirySecs config
    , createInvoiceNow = now
    }
  -- positional, as several wallet types share these field names
  return $! CreateInvoiceResult
    (createdInvoiceBolt11 created)
    (createdInvoicePaymentHash created)
    (createdInvoiceAmountSats created)
    (floor (createdInvoiceExpiresAt created))
