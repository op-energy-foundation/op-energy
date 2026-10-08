{-- | V2 pay invoice handler: pays an invoice out of the authenticated
 - account's balance.
 -
 - The balance is taken first and given back if the payment fails, the same
 - way a stake is refunded when posting an offer fails. The amount a client
 - sends is only a cap: an invoice this service issued is paid for the
 - amount it was created with.
 -}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE OverloadedStrings          #-}
module OpEnergy.Account.Server.V2.WalletAPI.PayInvoice
  ( payInvoiceHandler
  , payInvoice
  ) where

import           Control.Applicative ((<|>))
import           Control.Monad (when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Logger(logError)
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Except (ExceptT(..), throwE)
import           Control.Monad.Trans.Reader (ask)
import           Data.Text (Text)
import           Data.Text.Show (tshow)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Database.Persist (Entity(..))
import           Database.Persist.Postgresql (runSqlPersistMPool)

import qualified Data.OpEnergy.Account.API.V1.Account as API
import           Data.OpEnergy.Account.API.V2.LedgerReason (LedgerReason(..))
import           Data.OpEnergy.Account.API.V2.PayInvoiceRequest
                 ( PayInvoiceRequest(..)
                 )
import           Data.OpEnergy.Account.API.V2.PayInvoiceResult
                 ( PayInvoiceResult(..)
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
import           OpEnergy.Account.Server.V1.LedgerEntry
                 ( LedgerDirection(..), adjustBalanceTx
                 )
import           OpEnergy.Account.Server.V1.Wallet.Class
import           OpEnergy.Error
                 ( eitherThrowJSON, runExceptPrefixT
                 , CallstackError, accountNotFound, describeError
                 , walletAmountAboveMaximum, walletAmountRequired
                 , walletInvoiceNotFound, walletSelfPaymentNotAllowed
                 )
import           OpEnergy.ExceptMaybe(exceptTMaybeT)

-- | V2 wallet/pay endpoint
payInvoiceHandler
  :: API.AccountToken
  -> PayInvoiceRequest
  -> AppM PayInvoiceResult
payInvoiceHandler token request =
    let name = "V2.payInvoiceHandler"
    in profile name $ eitherThrowJSON
      ( runLogging . $(logError))
      $ payInvoice token request

-- | business logic for the V2 wallet/pay endpoint
payInvoice
  :: API.AccountToken
  -> PayInvoiceRequest
  -> AppM (Either CallstackError PayInvoiceResult)
payInvoice token (PayInvoiceRequest requestInvoice requestAmountSats) =
    let name = "V2.payInvoice"
    in profile name $ runExceptPrefixT name $ do
  (Entity key _) <- exceptTMaybeT accountNotFound
    $ mgetPersonByAccountToken token
  State{ config = config, accountDBPool = pool, wallet = walletV } <- lift ask
  -- the amount is needed before the payment, as the balance is taken first.
  -- An invoice this service issued is paid for the amount it was created
  -- with, whatever the client believes; for any other invoice the client has
  -- to say, as the wallet can not read it
  now <- liftIO getPOSIXTime
  mresolved <- ExceptT $ liftIO $ walletResolveInvoice walletV requestInvoice
  -- an invoice of this service, which has been paid already, is refused
  -- here rather than after the balance has been taken and given back
  when (maybe False (not . resolvedInvoicePayable) mresolved)
    $ throwE walletInvoiceNotFound
  -- an invoice of this account's own is refused here as well, for the same
  -- reason: the wallet answers Left for it, so every attempt took the
  -- balance and gave it straight back, which costs the caller nothing and
  -- leaves two ledger entries and two writes to the account's row each
  -- time. Both of those are in the account's own history, so repeating it
  -- fills the history it is read from
  when (maybe False ((== Just key) . resolvedInvoicePersonId) mresolved)
    $ throwE walletSelfPaymentNotAllowed
  amount <- exceptTMaybeT walletAmountRequired
    $ return (fmap resolvedInvoiceAmountSats mresolved <|> requestAmountSats)
  when (amount > configWalletMaxWithdrawalSats config)
    $ throwE walletAmountAboveMaximum
  balanceAfterDebit <- ExceptT $ liftIO $ flip runSqlPersistMPool pool
    $ adjustBalanceTx key Debit amount Withdrawal Nothing now
  epaid <- liftIO $ walletPayInvoice walletV $ PayInvoiceParams
    { payInvoicePersonId = key
    , payInvoiceBolt11 = requestInvoice
    , payInvoiceAmountSats = Just amount
    , payInvoiceNow = now
    }
  case epaid of
    Left err -> do
      -- the payment did not happen, so the balance goes straight back
      erefunded <- liftIO $ flip runSqlPersistMPool pool
        $ adjustBalanceTx key Credit amount Refund (Just "payment failed") now
      logUnreconciled
        ( "payment failed and the refund of " <> tshow amount
        <> " sats failed as well"
        ) erefunded
      throwE err
    Right paid -> do
      -- an invoice of this service stays inside it: the account it was
      -- created for is credited, as nothing left over lightning
      mapM_
        (\recipient -> do
          ecredited <- liftIO $ flip runSqlPersistMPool pool
            $ adjustBalanceTx
                recipient Credit (paidInvoiceAmountSats paid) Deposit Nothing now
          logUnreconciled
            ( "paid " <> tshow (paidInvoiceAmountSats paid)
            <> " sats, but crediting the receiving account failed"
            ) ecredited
        )
        (paidInvoiceRecipientPersonId paid)
      -- positional, as several wallet types share these field names
      return $! PayInvoiceResult
        (paidInvoicePaymentHash paid)
        (paidInvoiceAmountSats paid)
        (paidInvoiceFeeSats paid)
        balanceAfterDebit

-- | logs a balance change, which failed once the payment had already
-- happened. The request is answered as the payment went, so a failure here
-- leaves the ledger and the payment disagreeing and is written down rather
-- than returned to the caller
logUnreconciled
  :: Text
  -> Either CallstackError a
  -> ExceptT CallstackError AppM ()
logUnreconciled _ (Right _) = return ()
logUnreconciled context (Left err) = lift $ runLogging $ $(logError)
  ( "payInvoice: " <> context <> ", needs manual reconciliation: "
  <> describeError err
  )
