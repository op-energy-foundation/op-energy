{-- | How often one account may have this service call the wallet.
 -
 - An account is free to create and costs nothing to hold, while a request,
 - which reaches the wallet, costs this service a call whether or not it
 - ends in a payment: paying asks the wallet to read the invoice before any
 - balance is touched, and a request, which is refused afterwards, has
 - already spent that call. A wallet, which refuses this service for calling
 - too often, is refused for every account at once, so one account must not
 - be able to decide how often it is called.
 -
 - The count is kept per account, in memory, for the window it covers. It is
 - not a ledger: an attempt, which is lost when the service restarts, costs
 - the account nothing, and the wallet is protected again as soon as the
 - process is up. Keeping it in the DB would write a row per attempt, which
 - is the cost this module exists to avoid.
 -}
{-# LANGUAGE OverloadedStrings          #-}
module OpEnergy.Account.Server.V1.WalletRateLimit
  ( WalletAttempts
  , newWalletAttempts
  , takeWalletAttempt
  , forgetFinishedWindows
  )where

import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TVar (TVar)
import qualified Control.Concurrent.STM.TVar as TVar
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Time.Clock.POSIX (POSIXTime)
import           Data.Word (Word32)

import           OpEnergy.Account.Server.V1.Person (PersonId)

-- | how many attempts an account has made, and when the window, which
-- counts them, began. A window is dropped rather than slid: the account
-- gets its allowance back whole once the window it spent is over
data Window = Window
  { windowStartedAt :: !POSIXTime
  , windowAttempts :: !Word32
  }

-- | what every account has spent, in the window it is spending it in
newtype WalletAttempts = WalletAttempts (TVar (Map PersonId Window))

newWalletAttempts :: MonadIO m => m WalletAttempts
newWalletAttempts = fmap WalletAttempts $ liftIO $ TVar.newTVarIO Map.empty

-- | takes one attempt for the account, answering whether it had one left.
--
-- Read and write happen in one transaction, so two requests of the same
-- account, which arrive together, take two attempts rather than the same
-- one.
--
-- Example: with a limit of 10 and a window of 60, the eleventh attempt of
-- a minute answers False, and the first attempt after that minute answers
-- True with the count back at one
takeWalletAttempt
  :: MonadIO m
  => WalletAttempts
  -> Word32
  -- ^ how many attempts one account may make in a window
  -> POSIXTime
  -- ^ how long a window lasts
  -> POSIXTime
  -- ^ now
  -> PersonId
  -> m Bool
takeWalletAttempt (WalletAttempts attemptsV) limit window now key =
  liftIO $ atomically $ do
    attempts <- TVar.readTVar attemptsV
    let current = case Map.lookup key attempts of
          -- the window this account is counted in is still running, so
          -- it goes on being counted in that one
          Just w | windowStartedAt w + window > now -> w
          -- no window, or the one it had is over: a new one starts
          _ -> Window
            { windowStartedAt = now
            , windowAttempts = 0
            }
        taken = current { windowAttempts = windowAttempts current + 1 }
    if windowAttempts current >= limit
      then return False
      else do
        TVar.writeTVar attemptsV $! Map.insert key taken attempts
        return True

-- | drops the accounts, whose window is over, so the map holds the
-- accounts, which are spending now, rather than every account, which ever
-- paid. Run from the scheduler: an account, which is dropped, is counted
-- in a new window the next time it asks, which is what it would get by
-- staying
forgetFinishedWindows
  :: MonadIO m
  => WalletAttempts
  -> POSIXTime
  -> POSIXTime
  -> m ()
forgetFinishedWindows (WalletAttempts attemptsV) window now =
  liftIO $ atomically $ TVar.modifyTVar' attemptsV
    $ Map.filter (\w-> windowStartedAt w + window > now)
