{-- | Who is calling a trade chat endpoint, and whether they may moderate.
 -
 - This service does not know who a moderator is: the account service does.
 - It already resolves every account token there to find out who is calling,
 - and that answer carries the caller's role, so the check costs no second
 - call and no shared database.
 -}
{-# LANGUAGE OverloadedStrings          #-}
module OpEnergy.Offer.Server.V2.TelegramAPI.CheckModerator
  ( checkModerator
  , whoIsCalling
  ) where

import           Control.Monad (unless)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Except (ExceptT(..), throwE)
import           Prometheus (MonadMonitor)

import qualified Data.OpEnergy.Account.API.V1.Account as AccountAPI
import qualified Data.OpEnergy.Account.API.V1.UUID as AccountAPI
import           Data.OpEnergy.Account.API.V2.PersonRole (isModerator)
import qualified Data.OpEnergy.Account.API.V2.WhoAmIResult as AccountV2


import           OpEnergy.Offer.Server.V1.Class (AppT)
import qualified OpEnergy.Offer.Server.V1.AccountClient as AccountClient
import           OpEnergy.Error (CallstackError, notAModerator)

-- | refuses the call unless the token belongs to a moderator, and returns
-- that moderator's account, so a handler, which records who acted, does not
-- resolve the same token twice.
--
-- Asks isModerator rather than matching on one role, so an admin is not
-- quietly refused by a check, which only knows about moderators.
--
-- Example:
--
-- > moderatorUUID <- checkModerator token
checkModerator
  :: ( MonadIO m
     , MonadMonitor m
     )
  => AccountAPI.AccountToken
  -> ExceptT CallstackError (AppT m) (AccountAPI.UUID AccountAPI.Person)
checkModerator token = do
  (AccountV2.WhoAmIResult personUUIDV _displayName _balance role') <-
    ExceptT $ AccountClient.verifyAccountToken token
  unless (isModerator role') $ throwE notAModerator
  return $! personUUIDV

-- | the caller's account and the name it currently goes by.
--
-- The name is read here rather than stored once and left, so an account,
-- which connects after renaming itself, stores the new name.
--
-- Example:
--
-- > (personUUID, displayName) <- whoIsCalling token
whoIsCalling
  :: ( MonadIO m
     , MonadMonitor m
     )
  => AccountAPI.AccountToken
  -> ExceptT CallstackError (AppT m)
       (AccountAPI.UUID AccountAPI.Person, AccountAPI.DisplayName)
whoIsCalling token = do
  (AccountV2.WhoAmIResult personUUIDV displayNameV _balance _role) <-
    ExceptT $ AccountClient.verifyAccountToken token
  return (personUUIDV, displayNameV)
