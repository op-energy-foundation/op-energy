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
import           Control.Monad.Trans.Except (ExceptT(..), runExceptT)
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
import           Data.OpEnergy.Account.API.V1.Sats
                 ( Sats(..)
                 )
import           Data.OpEnergy.Account.API.V2.FundsKind
                 ( FundsKind(..)
                 )
import           OpEnergy.Account.Server.V1.Class
                 ( AppM, State(..), runLogging, profile)
import           OpEnergy.Account.Server.V1.LedgerEntry
                 ( LedgerDirection(..), adjustBalanceTx, freeSatsGrantedTx
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
--
-- A request may say that part of the amount is free sats: a payout of a
-- contract gives the winner their stake back in the pocket it was paid
-- from. The two are credited as two entries of one transaction, as one
-- entry can only name one pocket.
--
-- A payout's profit does not become real sats until the account is back
-- above the free sats it was given. The gift is the service's money, not
-- the account's: an account, which lost part of it and then won some back,
-- is still spending what it was given, so its winnings fill the free sats
-- up to the grant first and only what goes above the grant is real and
-- can be withdrawn. Deposits never pass through this -- somebody paid
-- those in, and they stay the account's to send.
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
      amount = amountSats request
      reasonV = fromMaybe Refund (reason request)
      freeSatsCredited = min amount $ fromMaybe (Sats 0) (freeSats request)
  nowUTC <- liftIO getCurrentTime
  -- the account is looked up in the same transaction as the change, so it
  -- cannot be gone between finding it and crediting it
  balance <- ExceptT $ liftIO $ flip runSqlPersistMPool pool $ runExceptT $ do
    (Entity key person) <- exceptTMaybeT accountNotFound
      $ selectFirst [ PersonUuid ==. modelUUID ] []
    -- the winner's own stake goes back to its pocket, and the profit on
    -- top of it tops the gift back up before any of it is real
    granted <- lift $ freeSatsGrantedTx key
    let profit = amount - freeSatsCredited
        freeAfterStakeBack = personFreeSats person + freeSatsCredited
        shortOfGrant = if granted > freeAfterStakeBack
          then granted - freeAfterStakeBack
          else Sats 0
        -- only a win made with free sats repays the gift. An account,
        -- which lost the gift, deposited its own sats and won with those,
        -- is playing with its own money: turning that profit into free
        -- sats would take what it paid in and make it unwithdrawable
        wonWithFreeSats = freeSatsCredited > Sats 0
        topUp = if reasonV == Winnings && wonWithFreeSats
          then min profit shortOfGrant
          else Sats 0
        freeCredited = freeSatsCredited + topUp
        realPart = amount - freeCredited
        credit part kind = ExceptT
          $ adjustBalanceTx key Credit part kind reasonV (reference request)
            (fmap modelApiUUIDPerson (counterparty request))
            (utcTimeToPOSIXSeconds nowUTC)
    if freeCredited > Sats 0 && realPart > Sats 0
      then credit freeCredited FreeSats >> credit realPart RealSats
      -- an amount of nothing still records an entry, as it did before the
      -- two pockets: it is the caller, which decides whether a movement of
      -- nothing is worth recording
      else credit amount
             (if freeCredited > Sats 0 then FreeSats else RealSats)
  return $! BalanceAdjustResult balance freeSatsCredited
