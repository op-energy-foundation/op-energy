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
import           Database.Persist (Entity(..), count, (==.), (>.))
import           Database.Persist.Postgresql (runSqlPersistMPool)

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
import           OpEnergy.Account.Server.V1.WalletPayment
                 ( EntityField(..), WalletPaymentDirection(..)
                 , WalletPaymentStatus(..)
                 )
import           OpEnergy.Error
                 ( eitherThrowJSON, runExceptPrefixT
                 , CallstackError, accountNotFound, dbQueryError
                 , eitherException
                 , walletAmountAboveMaximum, walletAmountBelowMinimum
                 , walletTooManyPendingInvoices
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
  State{ config = config, accountDBPool = pool, wallet = walletV } <- lift ask
  let amount = requestAmountSats -- named, as it is what the wallet is asked for
  when (amount < configWalletMinInvoiceSats config)
    $ throwE walletAmountBelowMinimum
  when (amount > configWalletMaxInvoiceSats config)
    $ throwE walletAmountAboveMaximum
  now <- liftIO getPOSIXTime
  -- an invoice, which is waiting, is asked about by the scheduler until it
  -- is paid or expires, so the number of them one account may have at once
  -- is what keeps an account -- and an account is free to create -- from
  -- deciding how often this service calls the wallet. An expired one is
  -- not counted: it is no longer asked about and the account would
  -- otherwise be left unable to ask to be paid
  ependingCount <- liftIO $ eitherException $ flip runSqlPersistMPool pool
    $ count
      [ WalletPaymentPersonId ==. key
      , WalletPaymentDirection ==. Incoming
      , WalletPaymentStatus ==. Pending
      , WalletPaymentExpiresAt >. Just now
      ]
  pendingCount <- either (const (throwE dbQueryError)) return ependingCount
  when (pendingCount >= fromIntegral (configWalletMaxPendingInvoices config))
    $ throwE walletTooManyPendingInvoices
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
