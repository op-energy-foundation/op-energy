{-- | V2 pay invoice handler: pays an invoice out of the authenticated
 - account's balance.
 -
 - The balance is taken first and given back if the payment fails, the same
 - way a stake is refunded when posting an offer fails. The amount a client
 - sends is only a cap: an invoice this service issued is paid for the
 - amount it was created with.
 -
 - A fee reserve is taken along with the amount, as what a payment costs is
 - only known once it has been made. What the fee does not use is given
 - back here, or by the scheduler for a payment, which is still in flight
 - when the wallet answers. A payment in flight is never given back: it may
 - still be paid, and refunding it would pay it twice.
 -}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE OverloadedStrings          #-}
module OpEnergy.Account.Server.V2.WalletAPI.PayInvoice
  ( payInvoiceHandler
  -- * exported to be tested
  , feeReserve
  , payInvoice
  ) where

import           Control.Applicative ((<|>))
import           Control.Monad (when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Logger(logError)
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Except (ExceptT(..), throwE)
import           Control.Monad.Trans.Reader (ask)
import           Data.Maybe (isJust)
import           Data.Text (Text)
import           Data.Text.Show (tshow)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Data.Word (Word64)
import           Database.Persist (Entity(..))
import           Database.Persist.Postgresql (runSqlPersistMPool)

import qualified Data.OpEnergy.Account.API.V1.Account as API
import           Data.OpEnergy.Account.API.V1.Sats (Sats(..))
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
import           OpEnergy.Account.Server.V1.Person
                 ( personBalance
                 )
import           OpEnergy.Account.Server.V1.Wallet.Class
import           OpEnergy.Account.Server.V1.WalletDeposits
                 ( creditDepositOnce
                 )
import           OpEnergy.Account.Server.V1.WalletRateLimit
                 ( takeWalletAttempt
                 )
import           OpEnergy.Error
                 ( eitherThrowJSON, runExceptPrefixT
                 , CallstackError, accountNotFound, describeError
                 , eitherException, insufficientBalance, unspecified
                 , walletAmountAboveMaximum, walletAmountBelowMinimum
                 , walletAmountRequired, walletInvoiceAlreadyPaid
                 , walletInvoiceNotFound, walletSelfPaymentNotAllowed
                 , walletTooManyPayAttempts
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
  (Entity key person) <- exceptTMaybeT accountNotFound
    $ mgetPersonByAccountToken token
  State{ config = config, accountDBPool = pool, wallet = walletV
       , walletAttempts = attemptsV } <- lift ask
  -- an account, which can not cover the smallest amount this wallet takes,
  -- can not pay any invoice. It is refused before the wallet is asked to
  -- read one: an account is free to create, so a request, which can not
  -- succeed, would otherwise be a way to have this service call the wallet
  -- as often as the caller likes
  when (personBalance person < configWalletMinInvoiceSats config)
    $ throwE insufficientBalance
  now <- liftIO getPOSIXTime
  -- every attempt asks the wallet to read the invoice before any balance
  -- is touched, and an attempt, which is refused after that, has already
  -- spent the call. An account is free to create, so the attempts one of
  -- them may make are counted: a wallet, which refuses this service for
  -- calling too often, is refused for every account at once
  allowed <- lift $ takeWalletAttempt attemptsV
    (configWalletMaxPayAttemptsPerWindow config)
    (configWalletMaxPayAttemptAccounts config)
    (fromIntegral (configWalletPayAttemptWindowSecs config))
    now key
  when (not allowed) $ throwE walletTooManyPayAttempts
  -- the amount is needed before the payment, as the balance is taken first.
  -- An invoice this service issued is paid for the amount it was created
  -- with, whatever the client believes; for any other invoice the client has
  -- to say, as the wallet can not read it
  mresolved <- ExceptT $ liftIO $ walletResolveInvoice walletV key requestInvoice
  -- an invoice this account has paid before is refused here too, for the
  -- same reason as the two checks below it: the wallet answers Left for it,
  -- so each attempt took the balance and gave it straight back, which costs
  -- the caller nothing and leaves two ledger entries every time
  when (maybe False resolvedInvoicePaidByAsker mresolved)
    $ throwE walletInvoiceAlreadyPaid
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
  -- the smallest amount the wallet handles applies to a payment as well as
  -- to an invoice: a payment of less, in particular of nothing at all, moves
  -- no balance, while it still records a payment and a ledger entry, so it
  -- would let a caller write rows without spending anything
  when (amount < configWalletMinInvoiceSats config)
    $ throwE walletAmountBelowMinimum
  when (amount > configWalletMaxWithdrawalSats config)
    $ throwE walletAmountAboveMaximum
  -- taken together with the amount, so a fee can not take an account
  -- below nothing: a balance, which does not cover both, refuses the
  -- payment here rather than leaving the fee unpaid for afterwards
  let reserve = feeReserve (configWalletFeeReservePercent config) amount
  -- the balance is the backend's to move: it takes the amount and the
  -- reserve in the transaction, which records the payment, and gives back
  -- what the payment did not use in the transaction, which finishes it.
  -- So a Left leaves the balance as it was and there is nothing to give
  -- back here, and a payment, which is recorded, has always been paid for
  paid <- ExceptT $ liftIO $ walletPayInvoice walletV $ PayInvoiceParams
    { payInvoicePersonId = key
    , payInvoiceBolt11 = requestInvoice
    , payInvoiceAmountSats = Just amount
    , payInvoiceFeeReserveSats = reserve
    , payInvoiceNow = now
    }
  -- nothing made the amount debited and the amount the wallet reports
  -- paying agree, and the interface lets a backend report its own: a
  -- difference is sats, which appeared or went with no ledger entry
  -- for them, so it is written down rather than passing unnoticed. It
  -- is not refused, as the payment has already happened by now. The
  -- mock always reports the amount it was asked for, so this is silent
  -- until a backend with a node does otherwise
  when (paidInvoiceAmountSats paid /= amount)
    $ lift $ runLogging $ $(logError)
      ( "payInvoice: debited " <> tshow (unSats amount)
      <> " sats while the wallet reports having paid "
      <> tshow (unSats (paidInvoiceAmountSats paid))
      <> " sats, which differ and need manual reconciliation"
      )
  -- an invoice of this service stays inside it: the account it was
  -- created for is credited, as nothing left over lightning. The credit
  -- claims the payment as it moves the balance, which is what keeps it
  -- to one: the scheduler credits every settled payment whose
  -- creditedAt is unset, so a credit written beside the claim is made
  -- again by the next tick
  when (isJust (paidInvoiceRecipientPersonId paid)) $ do
    -- guarded: the payment has already happened, so a pool failure here
    -- must be written down rather than thrown, which would answer the
    -- payer with a bare 500 and name nothing. creditWaitingDeposits
    -- credits it on the next tick either way
    ecredited <- fmap (either (Left . unspecified) id) $ liftIO
      $ eitherException $ flip runSqlPersistMPool pool
      $ creditDepositOnce (paidInvoicePaymentHash paid) now
    logUnreconciled
      ( "paid " <> tshow (unSats (paidInvoiceAmountSats paid))
      <> " sats, but crediting the receiving account failed"
      ) ecredited
  -- a fee above what was reserved is not taken from the account: it
  -- was told what it would pay, and the sats have left the wallet
  -- either way. The service is short by the difference, so it is
  -- written down here as the scheduler writes it down for a payment,
  -- which finishes later
  when (paidInvoiceFeeSats paid > reserve
        && paidInvoiceProgress paid == PaymentSettled)
    $ lift $ runLogging $ $(logError)
      ( "payInvoice: the payment cost "
      <> tshow (unSats (paidInvoiceFeeSats paid))
      <> " sats while " <> tshow (unSats reserve)
      <> " sats were reserved for it, so the wallet is short by the "
      <> "difference and needs manual reconciliation"
      )
  -- positional, as several wallet types share these field names
  return $! PayInvoiceResult
    (paidInvoicePaymentHash paid)
    (paidInvoiceAmountSats paid)
    (paidInvoiceFeeSats paid)
    (paidInvoiceBalanceAfter paid)
    (paidInvoiceProgress paid == PaymentSettled)

-- | what is taken on top of the amount to cover a fee, which is only
-- known once the payment has been made. It is a share of the amount, and
-- never nothing at all for an amount, which has one
feeReserve :: Word64 -> Sats -> Sats
feeReserve percent (Sats amount) =
  -- rounded up, so the share of a small amount is a satoshi rather than
  -- none, and a payment of nothing reserves nothing.
  --
  -- Worked out as Integer: the product of an amount and a percent
  -- overflows Word64 well below the amounts everifyWalletConfig admits,
  -- and the amount itself is bounded by WALLET_MAX_WITHDRAWAL_SATS, so
  -- the result always fits what it is converted back to
  Sats $! fromIntegral
    ((toInteger amount * toInteger percent + 99) `div` 100)

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
