{-- | V2 displayname handler: renames an account.
 -}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE OverloadedStrings          #-}
module OpEnergy.Account.Server.V2.AccountService.PostDisplayName
  ( postDisplayNameHandler
  , postDisplayName
  ) where

import           Control.Monad.Trans.Reader (ask)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Logger(logError)
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Except (throwE)
import           Data.Maybe (isJust)
import           Data.Time.Clock(getCurrentTime)
import           Data.Time.Clock.POSIX(utcTimeToPOSIXSeconds)

import           Database.Persist.Postgresql

import qualified Data.OpEnergy.Account.API.V1.Account as API
import           Data.OpEnergy.Account.API.V2.AccountInfo
                 ( AccountInfo(..)
                 )

import           OpEnergy.Account.Server.V1.Class
                 ( AppM, State(..), runLogging, profile)
import           OpEnergy.Account.Server.V1.AccountService
                 ( mgetPersonByAccountToken, mgetPersonByDisplayName)
import           OpEnergy.Account.Server.V1.Person

import           OpEnergy.Error
                 ( eitherThrowJSON, runExceptPrefixT
                 , CallstackError, accountNotFound, displayNameAlreadyTaken
                 )
import           OpEnergy.ExceptMaybe(exceptTMaybeT)


-- | V2 displayname endpoint
postDisplayNameHandler
  :: API.AccountToken
  -> API.DisplayName
  -> AppM AccountInfo
postDisplayNameHandler token newName =
    let name = "V2.postDisplayNameHandler"
    in profile name $ eitherThrowJSON
      ( runLogging . $(logError))
      $ postDisplayName token newName

-- | business logic for V2 displayname
postDisplayName
  :: API.AccountToken
  -> API.DisplayName
  -> AppM (Either CallstackError AccountInfo)
postDisplayName token newName =
    let name = "V2.postDisplayName"
    in profile name $ runExceptPrefixT name $ do
  State{ accountDBPool = pool
       } <- lift ask
  (Entity personKey person) <- exceptTMaybeT accountNotFound
    $ mgetPersonByAccountToken token
  -- check uniqueness
  mexists <- lift $ mgetPersonByDisplayName newName
  case mexists of
    Just _ -> throwE displayNameAlreadyTaken
    Nothing -> do
      liftIO $ flip runSqlPersistMPool pool $ do
        nowUTC <- liftIO getCurrentTime
        let now = utcTimeToPOSIXSeconds nowUTC
        update personKey
          [ PersonDisplayName =. newName
          , PersonLastUpdated =. now
          ]
      return $! AccountInfo newName (isJust (personHashedPassword person))
