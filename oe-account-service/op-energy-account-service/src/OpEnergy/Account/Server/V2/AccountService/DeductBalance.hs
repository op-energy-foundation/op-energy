{-- | V2 internal balance deduction handler: atomically deducts amountSats
 - from the given account's balance, failing (400) if it would go negative.
 - Internal-only -- see the X-Internal-Service-Secret header.
 -}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE OverloadedStrings          #-}
module OpEnergy.Account.Server.V2.AccountService.DeductBalance
  ( deductBalanceHandler
  , deductBalance
  ) where

import           Control.Monad (when)
import           Control.Monad.Trans.Reader (ask)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Logger(logError)
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Except (throwE)
import           Data.Int(Int64)
import           Data.Text(Text)
import           Data.Time.Clock(getCurrentTime)
import           Data.Time.Clock.POSIX(utcTimeToPOSIXSeconds)

import           Database.Persist.Postgresql

import           Data.OpEnergy.Account.API.V1.Sats
                 ( Sats(..)
                 )
import           Data.OpEnergy.Account.API.V2.BalanceAdjustRequest
                 ( BalanceAdjustRequest(..)
                 )
import           Data.OpEnergy.Account.API.V2.BalanceAdjustResult
                 ( BalanceAdjustResult(..)
                 )

import           OpEnergy.Account.Server.V1.Class
                 ( AppM, State(..), runLogging, profile)
import           OpEnergy.Account.Server.V1.Person

import           OpEnergy.Error
                 ( eitherThrowJSON, runExceptPrefixT
                 , CallstackError, accountNotFound, insufficientBalance
                 )
import           OpEnergy.ExceptMaybe(exceptTMaybeT)

import           OpEnergy.Account.Server.V2.AccountService.CheckInternalSecret
                 ( checkInternalServiceSecret
                 )


-- | V2 internal/balance/deduct endpoint
deductBalanceHandler
  :: Text
  -> BalanceAdjustRequest
  -> AppM BalanceAdjustResult
deductBalanceHandler secret request =
    let name = "V2.deductBalanceHandler"
    in profile name $ eitherThrowJSON
      ( runLogging . $(logError))
      $ deductBalance secret request

-- | business logic for V2 balance deduction. Uses updateWhereCount with
-- a balance guard to atomically reject underflow.
deductBalance
  :: Text
  -> BalanceAdjustRequest
  -> AppM (Either CallstackError BalanceAdjustResult)
deductBalance secret (BalanceAdjustRequest personUUIDV (Sats amountSats)) =
    let name = "V2.deductBalance"
    in profile name $ runExceptPrefixT name $ do
  checkInternalServiceSecret secret
  State{ accountDBPool = pool } <- lift ask
  let modelUUID = modelApiUUIDPerson personUUIDV
  (Entity key person) <- exceptTMaybeT accountNotFound
    $ liftIO $ flip runSqlPersistMPool pool
    $ selectFirst [ PersonUuid ==. modelUUID ] []
  deducted <- liftIO $ flip runSqlPersistMPool pool $ do
    nowUTC <- liftIO getCurrentTime
    let now = utcTimeToPOSIXSeconds nowUTC
    updateWhereCount
      [ PersonId ==. key, PersonBalance >=. Sats amountSats ]
      [ PersonBalance -=. Sats amountSats
      , PersonLastUpdated =. now
      ]
  when (deducted /= (1 :: Int64)) $ throwE insufficientBalance
  let Sats currentBalance = personBalance person
  return $! BalanceAdjustResult (Sats (currentBalance - amountSats))
