{-- | The check, which adds every account's free sats up against its ledger.
 -
 - An account's balance is kept in two pockets: free sats, which it was
 - given and can stake but never withdraw, and the rest, which it can. The
 - whole of the free sats rule rests on the first number being right, and it
 - is only right while every movement, which touched it, said so.
 -
 - So this adds the free part of an account's entries up and compares it
 - with what the account carries, and checks that the free part never
 - exceeds the balance. A mismatch means some path moved a balance without
 - saying which pocket it moved, which would let a gift be withdrawn, so it
 - is reported rather than waited for.
 -
 - It reads every account, so it runs on its own interval rather than on
 - every scheduler tick, and it asks the question as one aggregate: what
 - comes back is how many accounts disagree, not their rows.
 -}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module OpEnergy.Account.Server.V1.FreeSatsCheck
  ( checkFreeSatsAddUp
  ) where

import           Control.Concurrent.STM (atomically)
import qualified Control.Concurrent.STM.TVar as TVar
import           Control.Monad (when)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Logger (logError)
import           Control.Monad.Trans.Reader (ask)
import           Data.Int (Int64)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Database.Persist.Postgresql (runSqlPersistMPool)
import           Database.Persist.Sql (Single(..), rawSql)
import           Prometheus (MonadMonitor)

import           Data.Text.Show (tshow)

import           OpEnergy.Account.Server.V1.Class
                 ( AppT, State(..), profile, runLogging
                 )
import           OpEnergy.Account.Server.V1.Config (Config(..))

-- | reports any account whose free sats its ledger does not explain, at
-- most once per 'configFreeSatsCheckIntervalSecs'.
--
-- The time of the last run is claimed in the same transaction, which reads
-- it, so two ticks, which arrive together, do not both start a run: the one,
-- which claims the interval, runs it and the other finds the time already
-- moved. Claimed before the query rather than after it, so a run, which is
-- slow, does not let the next tick start a second one beside it.
checkFreeSatsAddUp :: (MonadIO m, MonadMonitor m) => AppT m ()
checkFreeSatsAddUp =
    let name = "V1.checkFreeSatsAddUp"
    in profile name $ do
  State{ config = config, accountDBPool = pool
       , freeSatsCheckedAt = checkedAtV } <- ask
  now <- liftIO getPOSIXTime
  let interval = fromIntegral (configFreeSatsCheckIntervalSecs config)
  claimed <- liftIO $ atomically $ do
    checkedAt <- TVar.readTVar checkedAtV
    if now - checkedAt >= interval
      then do
        TVar.writeTVar checkedAtV now
        return True
      else return False
  when claimed $ do
    rows <- liftIO $ flip runSqlPersistMPool pool $ rawSql
      "SELECT COUNT(*) FROM person p WHERE p.free_sats > p.balance\
      \ OR p.free_sats <> COALESCE((\
      \ SELECT SUM(CASE WHEN l.direction = 'credit'\
      \   THEN l.amount_sats ELSE -l.amount_sats END)\
      \  FROM ledger_entry l\
      \  WHERE l.person_id = p.id AND l.funds_kind = 'free'), 0)"
      []
    case rows of
      [ Single (0 :: Int64) ] -> return ()
      [ Single disagreeing ] -> runLogging $ $(logError)
        ( "checkFreeSatsAddUp: " <> tshow disagreeing
        <> " account(s) carry free sats their ledger does not explain:"
        <> " some balance moved without saying which pocket it moved"
        )
      _ -> runLogging $ $(logError)
        "checkFreeSatsAddUp: the count of disagreeing accounts came back\
        \ in a shape this does not understand"
