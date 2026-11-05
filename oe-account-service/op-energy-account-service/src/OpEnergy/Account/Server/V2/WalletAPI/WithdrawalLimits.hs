{-- | The limits on sending out more sats than an account has paid in.
 -
 - An account is given free sats, which it can stake but never withdraw.
 - What it wins on top of a stake is real sats, which it can. That is what
 - the product promises, and it is also the one way the gift can turn into
 - money: two accounts, which were both given free sats, take opposite sides
 - of the same contract, and whichever of them wins, the pair keeps the
 - profit as money.
 -
 - It cannot be made impossible without knowing who is behind an account, so
 - it is bounded instead. While an account has staked free sats at all, what
 - it may send beyond its own deposits has to clear four checks: a lifetime
 - cap, a playthrough of the gift, a number of different opponents, and a
 - hold after its last win. An account, which has only ever staked sats it
 - paid in, is not touched by any of them.
 -}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module OpEnergy.Account.Server.V2.WalletAPI.WithdrawalLimits
  ( checkWithdrawalLimitsTx
  ) where

import           Control.Monad (when)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Except (runExceptT, throwE)
import           Control.Monad.Trans.Reader (ReaderT)
import           Data.Int (Int64)
import           Data.Time.Clock.POSIX (POSIXTime)

import           Database.Persist
import           Database.Persist.Sql (SqlBackend, Single(..), rawSql)

import           Data.OpEnergy.Account.API.V1.Sats (Sats(..))
import           Data.OpEnergy.Account.API.V2.LedgerReason (LedgerReason(..))

import           OpEnergy.Account.Server.V1.Config
                 ( Config(..)
                 )
import           OpEnergy.Account.Server.V1.LedgerEntry
import           OpEnergy.Account.Server.V1.Person
import           OpEnergy.Error
                 ( CallstackError, dbQueryError
                 , withdrawalNeedsMoreOpponents, withdrawalOverProfitCap
                 , withdrawalPlaythroughNotMet, withdrawalWinningsOnHold
                 )

-- | what an account's ledger says about the four checks
data LedgerTotals = LedgerTotals
  { totalDepositedSats :: Int64
  , totalWithdrawnSats :: Int64
  , totalFreeStakedSats :: Int64
  , totalFreeGrantedSats :: Int64
  , distinctOpponents :: Int64
  }

-- | refuses a payment, which would send out more than the account has paid
-- in, unless the account has cleared every limit.
--
-- Run in the transaction, which is about to take the balance, so the totals
-- it reads are the ones the payment is weighed against.
--
-- Example:
--
-- > checkWithdrawalLimitsTx config key (Sats 20000) now
checkWithdrawalLimitsTx
  :: MonadIO m
  => Config
  -> PersonId
  -> Sats
  -> POSIXTime
  -> ReaderT SqlBackend m (Either CallstackError ())
checkWithdrawalLimitsTx config key (Sats amountSats) now = runExceptT $ do
  etotals <- lift $ readLedgerTotalsTx key
  totals <- either throwE return etotals
  let beyondDeposits = totalWithdrawnSats totals + fromIntegral amountSats
        - totalDepositedSats totals
  -- an account, which has never staked a free sat, can only be sending what
  -- it paid in or what it won with its own money: nothing to limit
  when (totalFreeStakedSats totals > 0 && beyondDeposits > 0) $ do
    when (beyondDeposits
          > fromIntegral (unSats (configWithdrawalProfitCapSats config)))
      $ throwE withdrawalOverProfitCap
    when (totalFreeStakedSats totals
          < totalFreeGrantedSats totals
            * fromIntegral (configWithdrawalFreeSatsPlaythrough config))
      $ throwE withdrawalPlaythroughNotMet
    when (distinctOpponents totals
          < fromIntegral (configWithdrawalMinDistinctOpponents config))
      $ throwE withdrawalNeedsMoreOpponents
    mlastWin <- lift $ selectFirst
      [ LedgerEntryPersonId ==. key, LedgerEntryReason ==. Winnings ]
      [ Desc LedgerEntryCreatedAt ]
    case mlastWin of
      Nothing -> return ()
      Just (Entity _ lastWin) ->
        when (now - ledgerEntryCreatedAt lastWin
              < fromIntegral (configWithdrawalWinningsHoldSecs config))
          $ throwE withdrawalWinningsOnHold

-- | the four totals, read in one pass over the account's ledger.
--
-- Written in SQL rather than with Persistent, as Persistent has no sum, no
-- count of distinct values and no filtered aggregate, and reading every
-- entry of an account to add them up in Haskell grows with its history.
readLedgerTotalsTx
  :: MonadIO m
  => PersonId
  -> ReaderT SqlBackend m (Either CallstackError LedgerTotals)
readLedgerTotalsTx key = do
  -- every sum is cast back to int8: SUM over a bigint column answers in
  -- numeric, which does not read back as an Int64
  rows <- rawSql
    "SELECT\
    \ COALESCE(SUM(CASE WHEN reason = 'deposit'\
    \   THEN amount_sats ELSE 0 END), 0)::int8,\
    \ COALESCE(SUM(CASE WHEN reason = 'withdrawal'\
    \   THEN amount_sats ELSE 0 END), 0)::int8,\
    \ COALESCE(SUM(CASE WHEN reason = 'stake' AND funds_kind = 'free'\
    \   THEN amount_sats ELSE 0 END), 0)::int8,\
    \ COALESCE(SUM(CASE WHEN reason = 'opening_balance' AND funds_kind = 'free'\
    \   THEN amount_sats ELSE 0 END), 0)::int8,\
    \ COUNT(DISTINCT counterparty) FILTER (WHERE reason = 'winnings')\
    \ FROM ledger_entry WHERE person_id = ?"
    [ toPersistValue key ]
  return $! case rows of
    [ ( Single deposited, Single withdrawn, Single freeStaked
      , Single freeGranted, Single opponents ) ] ->
        Right $! LedgerTotals deposited withdrawn freeStaked freeGranted
          opponents
    _ -> Left dbQueryError
