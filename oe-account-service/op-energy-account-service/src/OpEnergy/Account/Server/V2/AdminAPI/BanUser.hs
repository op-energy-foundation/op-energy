{-- | V2 ban handler: removes an account from the platform.
 -
 - A ban does three things in one transaction: it records when it happened,
 - so the account is refused from then on, and it bumps the logins count,
 - which is what the account token carries and is matched on, so every
 - token already handed out stops resolving. There is no session table to
 - clear: the counter is the session.
 -
 - What it deliberately does not do is touch the offer service. That
 - service owns the offers and the money in them, and nothing here can
 - reach across to it. It notices the ban on its next sweep and cancels the
 - account's open offers then, within a tick, so the ban itself is instant
 - and the refund follows.
 -}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE OverloadedStrings          #-}
module OpEnergy.Account.Server.V2.AdminAPI.BanUser
  ( banUserHandler
  , banUser
  ) where

import           Control.Monad (when)
import           Control.Monad.Trans.Reader (ask)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Logger (logError)
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Except (throwE)
import           Data.Time.Clock (getCurrentTime)
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

import           Database.Persist.Postgresql

import qualified Data.OpEnergy.Account.API.V1.Account as API
import           Data.OpEnergy.Account.API.V1.UUID (UUID)
import           Data.OpEnergy.Account.API.V2.BanUserResult
                 ( BanUserResult(..)
                 )

import           OpEnergy.Account.Server.V1.Class
                 ( AppM, State(..), runLogging, profile)
import           OpEnergy.Account.Server.V1.Person
import           OpEnergy.Account.Server.V2.AccountService.CheckModerator
                 ( checkModerator)

import           OpEnergy.Error
                 ( eitherThrowJSON, runExceptPrefixT
                 , CallstackError, accountNotFound, cannotBanSelf
                 )
import           OpEnergy.ExceptMaybe (exceptTMaybeT)

-- | V2 ban endpoint
banUserHandler
  :: UUID API.Person
  -> API.AccountToken
  -> AppM BanUserResult
banUserHandler targetUUID token =
    let name = "V2.banUserHandler"
    in profile name $ eitherThrowJSON
      ( runLogging . $(logError))
      $ banUser targetUUID token

-- | business logic for V2 ban
banUser
  :: UUID API.Person
  -> API.AccountToken
  -> AppM (Either CallstackError BanUserResult)
banUser targetUUID token =
    let name = "V2.banUser"
    in profile name $ runExceptPrefixT name $ do
  State{ accountDBPool = pool } <- lift ask
  (Entity _ moderator) <- checkModerator token
  let targetKeyUUID = modelApiUUIDPerson targetUUID
  -- a moderator, who bans their own account, locks themselves out with no
  -- way back, as there is no unban
  when (personUuid moderator == targetKeyUUID) $ throwE cannotBanSelf
  (Entity targetKey target) <- exceptTMaybeT accountNotFound
    $ liftIO $ flip runSqlPersistMPool pool
    $ selectFirst [ PersonUuid ==. targetKeyUUID ] []
  liftIO $ flip runSqlPersistMPool pool $ do
    nowUTC <- liftIO getCurrentTime
    let now = utcTimeToPOSIXSeconds nowUTC
    update targetKey
      [ PersonBannedAt =. Just now
      -- the token carries this counter and is matched on it, so raising it
      -- revokes every token this account already holds
      , PersonLoginsCount +=. 1
      , PersonLastUpdated =. now
      ]
  return $! BanUserResult
    (apiModelUUIDPerson $ personUuid target)
    (personDisplayName target)
