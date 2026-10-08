{-- | V2 mock wallet handler: marks an invoice of the authenticated account
 - paid, as a payer would.
 -
 - It exists so the wallet can be used before a lightning node does. It can
 - only be served while the mock wallet is in use: a real wallet offers no
 - way to mark an invoice paid, so there is nothing to call.
 -}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE OverloadedStrings          #-}
module OpEnergy.Account.Server.V2.WalletMockAPI.SettleInvoice
  ( settleInvoiceHandler
  , settleInvoice
  ) where

import           Control.Monad (when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Logger(logError)
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Except (ExceptT(..), throwE)
import           Control.Monad.Trans.Reader (ask)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Database.Persist (Entity(..), get)
import           Database.Persist.Postgresql (runSqlPersistMPool)

import qualified Data.OpEnergy.Account.API.V1.Account as API
import           Data.OpEnergy.Account.API.V2.LedgerReason (LedgerReason(..))
import           Data.OpEnergy.Account.API.V2.PaymentHash
                 ( PaymentHash(..)
                 )
import           Data.OpEnergy.Account.API.V2.SettleInvoiceRequest
                 ( SettleInvoiceRequest(..)
                 )
import           Data.OpEnergy.Account.API.V2.SettleInvoiceResult
                 ( SettleInvoiceResult(..)
                 )

import           OpEnergy.Account.Server.V1.AccountService
                 ( mgetPersonByAccountToken
                 )
import           OpEnergy.Account.Server.V1.Class
                 ( AppM, State(..), profile, runLogging
                 )
import           OpEnergy.Account.Server.V1.LedgerEntry
                 ( LedgerDirection(..), adjustBalanceTx, ledgerReference
                 )
import           OpEnergy.Account.Server.V1.Person
import           OpEnergy.Account.Server.V1.Wallet.Class
import           OpEnergy.Error
                 ( eitherThrowJSON, runExceptPrefixT
                 , CallstackError, accountNotFound, walletInvoiceNotFound
                 , walletSimulationNotSupported
                 )
import           OpEnergy.ExceptMaybe(exceptTMaybeT)

-- | V2 wallet/mock/settle endpoint
settleInvoiceHandler
  :: API.AccountToken
  -> SettleInvoiceRequest
  -> AppM SettleInvoiceResult
settleInvoiceHandler token request =
    let name = "V2.settleInvoiceHandler"
    in profile name $ eitherThrowJSON
      ( runLogging . $(logError))
      $ settleInvoice token request

-- | business logic for the V2 wallet/mock/settle endpoint. Credits the
-- balance only when this call is the one, which settled the invoice, so
-- settling twice credits once
settleInvoice
  :: API.AccountToken
  -> SettleInvoiceRequest
  -> AppM (Either CallstackError SettleInvoiceResult)
settleInvoice token (SettleInvoiceRequest requestPaymentHash) =
    let name = "V2.settleInvoice"
    in profile name $ runExceptPrefixT name $ do
  (Entity key _) <- exceptTMaybeT accountNotFound
    $ mgetPersonByAccountToken token
  State{ accountDBPool = pool, wallet = walletV } <- lift ask
  settleV <- exceptTMaybeT walletSimulationNotSupported
    $ return (walletSettleInvoice walletV)
  -- the account is given to the wallet rather than checked afterwards, so an
  -- invoice of somebody else is not found and stays untouched: checking after
  -- the call would already have settled it
  settled <- ExceptT $ liftIO $ settleV key requestPaymentHash
  -- kept as a second line of defence, so a backend, which ignores the
  -- account it was given, still settles nothing for the wrong one
  when (settledInvoicePersonId settled /= key) $ throwE walletInvoiceNotFound
  if not (settledInvoiceWasPending settled)
    then do
      -- the balance is read again here rather than taken from the account
      -- lookup at the start of this request. A settle, which finds the
      -- invoice already settled, is normally a repeat of the one, which
      -- credited it, and reporting the balance from before that credit
      -- states the pre-deposit balance as though it were the balance the
      -- deposit produced
      currentBalance <- exceptTMaybeT accountNotFound
        $ liftIO $ flip runSqlPersistMPool pool
        $ fmap (fmap personBalance) $ get key
      -- positional, as several wallet types share these field names
      return $! SettleInvoiceResult
        (settledInvoicePaymentHash settled)
        (settledInvoiceAmountSats settled)
        currentBalance
    else do
      now <- liftIO getPOSIXTime
      -- the payment this credit is for, so a settled payment with no
      -- matching entry can be found: the credit is a transaction of its own
      -- after the invoice has been marked settled, and nothing re-runs it,
      -- so the two can disagree and an operator needs to be able to join
      -- them
      balance <- ExceptT $ liftIO $ flip runSqlPersistMPool pool
        $ adjustBalanceTx
            key Credit (settledInvoiceAmountSats settled) Deposit
            ( Just $ ledgerReference "payment"
              $ unPaymentHash (settledInvoicePaymentHash settled)
            )
            now
      return $! SettleInvoiceResult
        (settledInvoicePaymentHash settled)
        (settledInvoiceAmountSats settled)
        balance
