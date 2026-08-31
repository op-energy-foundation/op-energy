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
import           Control.Monad.Trans.Except (throwE)
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
                 , CallstackError, accountNotFound
                 )

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

-- | business logic for V2 balance credit
creditBalance
  :: Text
  -> BalanceAdjustRequest
  -> AppM (Either CallstackError BalanceAdjustResult)
creditBalance secret (BalanceAdjustRequest personUUIDV amountSats) =
    let name = "V2.creditBalance"
    in profile name $ runExceptPrefixT name $ do
  checkInternalServiceSecret secret
  State{ accountDBPool = pool } <- lift ask
  let modelUUID = modelApiUUIDPerson personUUIDV
  mperson <- liftIO $ flip runSqlPersistMPool pool $
    selectFirst [ PersonUuid ==. modelUUID ] []
  case mperson of
    Nothing -> throwE accountNotFound
    Just (Entity key person) -> do
      liftIO $ flip runSqlPersistMPool pool $
        update key [ PersonBalance +=. amountSats ]
      return $! BalanceAdjustResult (personBalance person + amountSats)
