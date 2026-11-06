{-- | The limits on sending out what the gift won.
 -
 - An account is given free sats, which it can stake but never withdraw.
 - What they win on top of a stake is real money, and that is the one way
 - the gift can turn into money: two accounts, which were both given free
 - sats, take opposite sides of the same contract, and whichever of them
 - wins, the pair keeps the profit.
 -
 - It cannot be made impossible without knowing who is behind an account, so
 - it is bounded instead. Sats, which the gift won, are held in their own
 - pocket, and they are moved into the account's own money only once four
 - checks are cleared: a lifetime cap, a playthrough of the gift, a number
 - of different opponents, and a hold on winnings, which are still fresh.
 -
 - Only that pocket is guarded. Sats, which were paid in, and sats won while
 - staking them, are the account's own money and leave whenever it asks --
 - an account, which deposits and plays with what it deposited, is not
 - touched by any of this.
 -}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module OpEnergy.Account.Server.V2.WalletAPI.WithdrawalLimits
  ( releaseForWithdrawalTx
  ) where

import           Control.Monad (when)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Except (ExceptT(..), runExceptT, throwE)
import           Control.Monad.Trans.Reader (ReaderT)
import           Data.Int (Int64)
import           Data.Text (Text)
import           Data.Time.Clock.POSIX (POSIXTime)

import           Database.Persist (get, toPersistValue)
import           Database.Persist.Sql (SqlBackend, Single(..), rawSql)

import           Data.OpEnergy.Account.API.V1.Sats (Sats(..))
import           Data.OpEnergy.Account.API.V2.FundsKind (FundsKind(..))
import           Data.OpEnergy.Account.API.V2.LedgerReason (LedgerReason(..))

import           OpEnergy.Account.Server.V1.Config
                 ( Config(..)
                 )
import           OpEnergy.Account.Server.V1.LedgerEntry (releaseGiftWonTx)
import           OpEnergy.Account.Server.V1.Person
                 ( PersonId, ownOf, personGiftWonSats
                 )
import           OpEnergy.Error
                 ( CallstackError, accountNotFound, dbQueryError
                 , insufficientBalance
                 , withdrawalNeedsMoreOpponents, withdrawalOverProfitCap
                 , withdrawalPlaythroughNotMet, withdrawalWinningsOnHold
                 )
import           OpEnergy.ExceptMaybe (exceptTMaybeT)

-- | what an account's ledger says about the four checks.
--
-- Signed, although no sat is ever negative: the checks subtract one total
-- from another, and a difference, which goes below nothing, has to read as
-- less than nothing rather than as enormous.
data LedgerTotals = LedgerTotals
  { totalFreeStakedSats :: Int64
  , totalFreeGrantedSats :: Int64
  , distinctOpponents :: Int64
  , totalReleasedSats :: Int64
    -- ^ what has already been moved out of the gift's winnings into the
    -- account's own money: the running total the cap and the hold are
    -- measured against
  , matureGiftWonSats :: Int64
    -- ^ what the gift won longer ago than the hold, so it has waited
  }

-- | moves as much of a payment as the account's own money does not cover
-- out of what the gift won, once the four checks are cleared, so the
-- payment, which follows, spends its own money like any other.
--
-- A payment, which the account's own money covers, releases nothing and
-- clears nothing: those sats were paid in, or won while staking sats, which
-- were paid in, and they are the account's to send.
--
-- Run in the transaction, which is about to take the balance, so the totals
-- it reads are the ones the payment is weighed against.
--
-- Example:
--
-- > releaseForWithdrawalTx config key (Sats 20000) (Just "payment:7") now
releaseForWithdrawalTx
  :: MonadIO m
  => Config
  -> PersonId
  -> Sats
  -> Maybe Text
  -> POSIXTime
  -> ReaderT SqlBackend m (Either CallstackError ())
releaseForWithdrawalTx config key amount reference now = runExceptT $ do
  person <- exceptTMaybeT accountNotFound $ get key
  let own = ownOf person
  -- the account's own money covers it: nothing of the gift's is being sent
  when (amount > own) $ do
    let shortfall = amount - own
    when (shortfall > personGiftWonSats person) $ throwE insufficientBalance
    -- winnings, which are younger than this, have not waited yet
    let matureBefore = now
          - fromIntegral (configWithdrawalWinningsHoldSecs config)
    etotals <- lift $ readLedgerTotalsTx key matureBefore
    totals <- either throwE return etotals
    let released = totalReleasedSats totals + fromIntegral (unSats shortfall)
    when (released
          > fromIntegral (unSats (configWithdrawalProfitCapSats config)))
      $ throwE withdrawalOverProfitCap
    when (totalFreeStakedSats totals
          < totalFreeGrantedSats totals
            * fromIntegral (configWithdrawalFreeSatsPlaythrough config))
      $ throwE withdrawalPlaythroughNotMet
    when (distinctOpponents totals
          < fromIntegral (configWithdrawalMinDistinctOpponents config))
      $ throwE withdrawalNeedsMoreOpponents
    -- the hold is on each win rather than on the account: what has been
    -- released has to be covered by the wins, which have already waited.
    -- Reading the last win instead would start the wait again at every
    -- win, so an account, which wins often enough, could never send at all
    when (released > matureGiftWonSats totals)
      $ throwE withdrawalWinningsOnHold
    ExceptT $ releaseGiftWonTx key shortfall reference now

-- | the totals, read in one pass over the account's ledger.
--
-- Written in SQL rather than with Persistent, as Persistent has no sum, no
-- count of distinct values and no filtered aggregate, and reading every
-- entry of an account to add them up in Haskell grows with its history.
--
-- The reasons and funds kinds are passed as parameters rather than written
-- into the query, so they are serialised by the same instances, which wrote
-- the rows: a renamed constructor cannot leave this silently summing
-- nothing.
readLedgerTotalsTx
  :: MonadIO m
  => PersonId
  -> POSIXTime
  -> ReaderT SqlBackend m (Either CallstackError LedgerTotals)
readLedgerTotalsTx key matureBefore = do
  -- every sum is cast back to int8: SUM over a bigint column answers in
  -- numeric, which does not read back as an Int64
  rows <- rawSql
    "SELECT\
    \ COALESCE(SUM(CASE WHEN reason = ? AND funds_kind = ?\
    \   THEN amount_sats ELSE 0 END), 0)::int8,\
    \ COALESCE(SUM(CASE WHEN reason = ? AND funds_kind = ?\
    \   THEN amount_sats ELSE 0 END), 0)::int8,\
    \ COUNT(DISTINCT counterparty) FILTER (WHERE reason = ?),\
    \ COALESCE(SUM(CASE WHEN reason = ? AND funds_kind = ?\
    \   THEN amount_sats ELSE 0 END), 0)::int8,\
    \ COALESCE(SUM(CASE WHEN reason = ? AND funds_kind = ?\
    \   AND created_at <= ? THEN amount_sats ELSE 0 END), 0)::int8\
    \ FROM ledger_entry WHERE person_id = ?"
    [ toPersistValue Stake, toPersistValue FreeSats
    , toPersistValue OpeningBalance, toPersistValue FreeSats
    , toPersistValue Winnings
    , toPersistValue Release, toPersistValue GiftWonSats
    , toPersistValue Winnings, toPersistValue GiftWonSats
    , toPersistValue matureBefore
    , toPersistValue key
    ]
  return $! case rows of
    [ ( Single freeStaked, Single freeGranted, Single opponents
      , Single released, Single matureGiftWon ) ] ->
        Right $! LedgerTotals freeStaked freeGranted opponents released
          matureGiftWon
    _ -> Left dbQueryError
