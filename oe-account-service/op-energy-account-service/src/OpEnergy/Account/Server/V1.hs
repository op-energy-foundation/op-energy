{-- |
 - This module is the top module of backend V1
 -}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DuplicateRecordFields      #-}
module OpEnergy.Account.Server.V1
  ( schedulerIteration
  , accountServer
  )where

import           Servant
import           Control.Monad.IO.Class(MonadIO, liftIO)
import           Control.Monad.Logger (logError)
import           Control.Monad.Trans.Reader (ask)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Database.Persist.Postgresql (runSqlPersistMPool)

import           Data.OpEnergy.Account.API.V1
import           Data.OpEnergy.API.V1(GitHashResponse(..))
import qualified OpEnergy.Account.Server.GitCommitHash as Server
import           OpEnergy.Account.Server.V1.Class
                 ( AppT, State(..), runLogging
                 )
import           OpEnergy.Account.Server.V1.AccountService
import           OpEnergy.Account.Server.V1.Config (Config(..))
import           OpEnergy.Account.Server.V1.WalletDeposits
                 ( creditWaitingDeposits
                 )
import           OpEnergy.Account.Server.V1.WalletInvoiceReservation
                 ( dropStaleInvoiceSlotsTx, invoiceReservationTtl
                 )
import           OpEnergy.Account.Server.V1.WalletRateLimit
                 ( forgetFinishedWindows
                 )

import           OpEnergy.Error (eitherException)
import           Prometheus(MonadMonitor)

-- | this is the implementation of OpEnergy.Account.API.V1.AccountV1API. Check this type for the reference and API
-- documentation
accountServer :: ServerT AccountV1API (AppT Handler)
accountServer
  = OpEnergy.Account.Server.V1.AccountService.register Nothing
  :<|> OpEnergy.Account.Server.V1.AccountService.login
  :<|> OpEnergy.Account.Server.V1.AccountService.postDisplayName
  :<|> oeGitHashGet

-- | one iteration that called from scheduler thread
schedulerIteration :: (MonadIO m, MonadMonitor m) => AppT m ()
schedulerIteration = do
  forgetSpentPayAttempts
  dropStaleInvoiceSlots
  creditWaitingDeposits

-- | deletes the slots, which were taken for an invoice, which was being
-- made, and never given back. They have already stopped counting against
-- the account by the time this runs, so this only keeps the table small
dropStaleInvoiceSlots :: (MonadIO m, MonadMonitor m) => AppT m ()
dropStaleInvoiceSlots = do
  State{ accountDBPool = pool } <- ask
  now <- liftIO getPOSIXTime
  -- guarded, as this runs on the scheduler thread and the process waits
  -- on any of its threads ending: an exception here would take the API
  -- down with it rather than skipping a tick
  edropped <- liftIO $ eitherException $ flip runSqlPersistMPool pool
    $ dropStaleInvoiceSlotsTx invoiceReservationTtl now
  case edropped of
    Right () -> return ()
    Left err -> runLogging $ $(logError)
      ( "dropStaleInvoiceSlots: could not delete the slots, which have "
      <> "stopped counting: " <> err
      )

-- | drops the accounts, whose window of payment attempts is over, so what
-- counts them holds the accounts, which are paying now, rather than every
-- account, which ever paid
forgetSpentPayAttempts :: MonadIO m => AppT m ()
forgetSpentPayAttempts = do
  State{ config = config, walletAttempts = attemptsV } <- ask
  now <- liftIO getPOSIXTime
  forgetFinishedWindows attemptsV
    (fromIntegral (configWalletPayAttemptWindowSecs config))
    now

-- returns just commit hash, provided by build system
oeGitHashGet :: AppT Handler GitHashResponse
oeGitHashGet = return $ GitHashResponse
  { gitCommitHash = Server.gitCommitHash
  }
