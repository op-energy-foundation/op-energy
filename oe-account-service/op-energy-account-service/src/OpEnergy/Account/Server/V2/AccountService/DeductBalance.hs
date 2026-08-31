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

import           Control.Monad.Trans.Reader (ask)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Logger(logError)
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Except (throwE)
import           Data.Int(Int64)
import           Data.Text(Text)

import           Database.Persist.Postgresql

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

-- | business logic for V2 balance deduction
deductBalance
  :: Text
  -> BalanceAdjustRequest
  -> AppM (Either CallstackError BalanceAdjustResult)
deductBalance secret (BalanceAdjustRequest personUUIDV amountSats) =
    let name = "V2.deductBalance"
    in profile name $ runExceptPrefixT name $ do
  checkInternalServiceSecret secret
  State{ accountDBPool = pool } <- lift ask
  let modelUUID = modelApiUUIDPerson personUUIDV
  mperson <- liftIO $ flip runSqlPersistMPool pool $
    selectFirst [ PersonUuid ==. modelUUID ] []
  case mperson of
    Nothing -> throwE accountNotFound
    Just (Entity key person) -> do
      deducted <- liftIO $ flip runSqlPersistMPool pool $
        updateWhereCount
          [ PersonId ==. key, PersonBalance >=. amountSats ]
          [ PersonBalance -=. amountSats ]
      if deducted /= (1 :: Int64)
        then throwE insufficientBalance
        else return $! BalanceAdjustResult (personBalance person - amountSats)
