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
import           OpEnergy.Account.Server.V1.WalletDeposits
                 ( creditDepositOnce
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
  -- creditedAt, not whether this call was the one, which settled the
  -- invoice, is what decides: a payment, which is settled but not yet
  -- credited, is still waiting for its credit, so a retry of a settle whose
  -- credit was lost between the two transactions credits it here rather
  -- than reporting a balance, which never included it. Whichever caller
  -- claims it first is the one, which credits
  now <- liftIO getPOSIXTime
  mbalance <- ExceptT $ liftIO $ flip runSqlPersistMPool pool
    $ creditDepositOnce (settledInvoicePaymentHash settled) now
  balance <- case mbalance of
    Just credited -> return credited
    -- somebody else had credited it, so the balance is read as it now is
    Nothing -> exceptTMaybeT accountNotFound
      $ liftIO $ flip runSqlPersistMPool pool
      $ fmap (fmap personBalance) $ get key
  -- positional, as several wallet types share these field names
  return $! SettleInvoiceResult
    (settledInvoicePaymentHash settled)
    (settledInvoiceAmountSats settled)
    balance
