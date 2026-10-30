{-- | V2 internal balance credit handler: unconditionally credits amountSats
 - to the given account's balance. Internal-only -- see the
 - X-Internal-Service-Secret header.
 -}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE OverloadedStrings          #-}
module OpEnergy.Account.Server.V2.AccountService.CreditBalance
  ( creditBalanceHandler
  , creditBalance
  ) where

import           Control.Monad.Trans.Reader (ask)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Logger(logError)
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Except (ExceptT(..))
import           Data.Maybe(fromMaybe)
import           Data.Text(Text)
import           Data.Time.Clock(getCurrentTime)
import           Data.Time.Clock.POSIX(utcTimeToPOSIXSeconds)

import           Database.Persist.Postgresql

import           Data.OpEnergy.Account.API.V2.LedgerReason
                 ( LedgerReason(..)
                 )
import           Data.OpEnergy.Account.API.V2.BalanceAdjustRequest
                 ( BalanceAdjustRequest(..)
                 )
import           Data.OpEnergy.Account.API.V2.BalanceAdjustResult
                 ( BalanceAdjustResult(..)
                 )

import           OpEnergy.Account.Server.V1.Class
                 ( AppM, State(..), runLogging, profile)
import           OpEnergy.Account.Server.V1.LedgerEntry
                 ( LedgerDirection(..), adjustBalanceTx
                 )
import           OpEnergy.Account.Server.V1.Person

import           OpEnergy.Error
                 ( eitherThrowJSON, runExceptPrefixT
                 , CallstackError, accountNotFound
                 )
import           OpEnergy.ExceptMaybe(exceptTMaybeT)

import           OpEnergy.Account.Server.V2.AccountService.CheckInternalSecret
                 ( checkInternalServiceSecret
                 )


-- | V2 internal/balance/credit endpoint
creditBalanceHandler
  :: Text
  -> BalanceAdjustRequest
  -> AppM BalanceAdjustResult
creditBalanceHandler secret request =
    let name = "V2.creditBalanceHandler"
    in profile name $ eitherThrowJSON
      ( runLogging . $(logError))
      $ creditBalance secret request

-- | business logic for V2 balance credit. Credits through
-- 'adjustBalanceTx', which records why the balance changed and returns the
-- balance after the credit, read in the same transaction as the update. A
-- request without a reason records a refund, which is what a caller
-- predating the ledger credits for.
creditBalance
  :: Text
  -> BalanceAdjustRequest
  -> AppM (Either CallstackError BalanceAdjustResult)
creditBalance secret request =
    let name = "V2.creditBalance"
    in profile name $ runExceptPrefixT name $ do
  checkInternalServiceSecret secret
  State{ accountDBPool = pool } <- lift ask
  let modelUUID = modelApiUUIDPerson (personUUID request)
  (Entity key _) <- exceptTMaybeT accountNotFound
    $ liftIO $ flip runSqlPersistMPool pool
    $ selectFirst [ PersonUuid ==. modelUUID ] []
  nowUTC <- liftIO getCurrentTime
  balance <- ExceptT $ liftIO $ flip runSqlPersistMPool pool
    $ adjustBalanceTx
        key
        Credit
        (amountSats request)
        (fromMaybe Refund (reason request))
        (reference request)
        (utcTimeToPOSIXSeconds nowUTC)
  return $! BalanceAdjustResult balance
