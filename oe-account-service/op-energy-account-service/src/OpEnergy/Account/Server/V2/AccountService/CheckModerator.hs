{-- | Shared guard for the endpoints only a moderator may call: resolves
 - the caller's account token and refuses the call unless that account
 - holds the moderator powers.
 -
 - Written once and called as the first line of each such logic function,
 - so a new moderator endpoint cannot be written without the check, and the
 - check cannot drift between them.
 -}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleContexts           #-}
module OpEnergy.Account.Server.V2.AccountService.CheckModerator
  ( checkModerator
  ) where

import           Control.Monad (unless)
import           Control.Monad.Trans.Except (ExceptT, throwE)
import           Prometheus (MonadMonitor)
import           Control.Monad.IO.Class (MonadIO)

import           Database.Persist (Entity(..))

import qualified Data.OpEnergy.Account.API.V1.Account as API
import           Data.OpEnergy.Account.API.V2.PersonRole (isModerator)

import           OpEnergy.Account.Server.V1.Class (AppT)
import           OpEnergy.Account.Server.V1.AccountService
                 ( mgetPersonByAccountToken)
import           OpEnergy.Account.Server.V1.Person

import           OpEnergy.Error
                 ( CallstackError, accountNotFound, notAModerator
                 )
import           OpEnergy.ExceptMaybe (exceptTMaybeT)

-- | refuses the call unless the token belongs to a moderator, and returns
-- that moderator so the handler can tell it apart from the account it is
-- acting on without looking the same account up a second time.
--
-- A banned account is not found by 'mgetPersonByAccountToken' at all, so a
-- banned moderator is refused here as an unknown account.
--
-- Example:
--
-- > (Entity _ moderator) <- checkModerator token
checkModerator
  :: (MonadIO m, MonadMonitor m)
  => API.AccountToken
  -> ExceptT CallstackError (AppT m) (Entity Person)
checkModerator token = do
  person@(Entity _ moderator) <- exceptTMaybeT accountNotFound
    $ mgetPersonByAccountToken token
  unless (isModerator (personRole moderator)) $ throwE notAModerator
  return person
