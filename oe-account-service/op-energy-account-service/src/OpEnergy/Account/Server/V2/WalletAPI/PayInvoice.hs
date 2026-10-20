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
import           Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import           Database.Persist (Entity(..))
import           Database.Persist.Postgresql (runSqlPersistMPool)

import qualified Data.OpEnergy.Account.API.V1.Account as API
import           Data.OpEnergy.Account.API.V1.Sats (Sats(..))
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
import           OpEnergy.Account.Server.V1.Person
                 ( PersonId, personBalance
                 )
import           OpEnergy.Account.Server.V1.Wallet.Class
import           OpEnergy.Account.Server.V1.WalletDeposits
                 ( creditDepositOnce
                 )
import           OpEnergy.Error
                 ( eitherThrowJSON, runExceptPrefixT
                 , CallstackError, accountNotFound, describeError
                 , walletAmountAboveMaximum, walletAmountBelowMinimum
                 , insufficientBalance
                 , walletAmountRequired, walletInvoiceAlreadyPaid
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
  (Entity key person) <- exceptTMaybeT accountNotFound
    $ mgetPersonByAccountToken token
  State{ config = config, accountDBPool = pool, wallet = walletV } <- lift ask
  -- an account, which can not cover the smallest amount this wallet takes,
  -- can not pay any invoice. It is refused before the wallet is asked to
  -- read one: an account is free to create, so a request, which can not
  -- succeed, would otherwise be a way to have this service call the wallet
  -- as often as the caller likes
  when (personBalance person < configWalletMinInvoiceSats config)
    $ throwE insufficientBalance
  -- the amount is needed before the payment, as the balance is taken first.
  -- An invoice this service issued is paid for the amount it was created
  -- with, whatever the client believes; for any other invoice the client has
  -- to say, as the wallet can not read it
  now <- liftIO getPOSIXTime
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
  let reserve = feeReserve config amount
      taken = amount + reserve
  balanceAfterDebit <- ExceptT $ liftIO $ flip runSqlPersistMPool pool
    $ adjustBalanceTx key Debit taken Withdrawal Nothing now
  epaid <- liftIO $ walletPayInvoice walletV $ PayInvoiceParams
    { payInvoicePersonId = key
    , payInvoiceBolt11 = requestInvoice
    , payInvoiceAmountSats = Just amount
    , payInvoiceFeeReserveSats = reserve
    , payInvoiceNow = now
    }
  case epaid of
    Left err -> do
      -- the payment did not happen, so the balance goes straight back,
      -- reserve and all. A backend only answers this way for a payment,
      -- which can not still be travelling
      erefunded <- liftIO $ flip runSqlPersistMPool pool
        $ adjustBalanceTx key Credit taken Refund (Just "payment failed") now
      logUnreconciled
        ( "payment failed and the refund of " <> tshow (unSats taken)
        <> " sats failed as well"
        ) erefunded
      throwE err
    Right paid -> do
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
        ecredited <- liftIO $ flip runSqlPersistMPool pool
          $ creditDepositOnce (paidInvoicePaymentHash paid) now
        logUnreconciled
          ( "paid " <> tshow (unSats (paidInvoiceAmountSats paid))
          <> " sats, but crediting the receiving account failed"
          ) ecredited
      -- what the fee did not use goes back, once the fee is known. A
      -- payment, which is still in flight, has no fee yet: the whole
      -- reserve stays taken and the scheduler releases it when the wallet
      -- says what the payment cost
      balanceAfterFee <- case paidInvoiceProgress paid of
        PaymentInFlight -> return balanceAfterDebit
        PaymentSettled -> releaseReserve key reserve
          (paidInvoiceFeeSats paid) balanceAfterDebit now
      -- positional, as several wallet types share these field names
      return $! PayInvoiceResult
        (paidInvoicePaymentHash paid)
        (paidInvoiceAmountSats paid)
        (paidInvoiceFeeSats paid)
        balanceAfterFee

-- | what is taken on top of the amount to cover a fee, which is only
-- known once the payment has been made. It is a share of the amount, and
-- never nothing at all for an amount, which has one
feeReserve :: Config -> Sats -> Sats
feeReserve config (Sats amount) =
  -- rounded up, so the share of a small amount is a satoshi rather than
  -- none, and a payment of nothing reserves nothing
  Sats ((amount * configWalletFeeReservePercent config + 99) `div` 100)

-- | gives back the part of the reserve, which the fee did not use.
--
-- A fee above the reserve is not taken further: the sats are already gone
-- from the wallet, and taking more from an account, which was told what it
-- would pay, is worse than recording that the reserve was too small
releaseReserve
  :: PersonId
  -> Sats
  -> Sats
  -> Sats
  -> POSIXTime
  -> ExceptT CallstackError AppM Sats
releaseReserve key reserve fee balanceAfterDebit now
  | fee > reserve = do
      lift $ runLogging $ $(logError)
        ( "payInvoice: the payment cost " <> tshow (unSats fee)
        <> " sats while " <> tshow (unSats reserve)
        <> " sats were reserved for it, so the wallet is short by the "
        <> "difference and needs manual reconciliation"
        )
      return balanceAfterDebit
  | otherwise = do
      State{ accountDBPool = pool } <- lift ask
      let unused = reserve - fee
      ereleased <- liftIO $ flip runSqlPersistMPool pool
        $ adjustBalanceTx key Credit unused Refund
          (Just "fee reserve not used") now
      logUnreconciled
        ( "paid, but giving back the " <> tshow (unSats unused)
        <> " sats of the fee reserve, which were not used, failed"
        ) ereleased
      return $! either (const balanceAfterDebit) id ereleased

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
