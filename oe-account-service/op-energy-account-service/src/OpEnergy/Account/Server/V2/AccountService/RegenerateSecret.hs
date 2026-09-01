{-- | V2 secret regeneration handler: replaces the caller's account secret
 - with a freshly generated one and returns it. The previous secret stops
 - working immediately, revoking any shared or lost secret link.
 -}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE OverloadedStrings          #-}
module OpEnergy.Account.Server.V2.AccountService.RegenerateSecret
  ( regenerateSecretHandler
  , regenerateSecret
  ) where

import           Control.Monad.Trans.Reader (ask)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Logger(logError)
import           Control.Monad.Trans (lift)
import           Data.Time.Clock(getCurrentTime)
import           Data.Time.Clock.POSIX(utcTimeToPOSIXSeconds)

import           Database.Persist.Postgresql

import qualified Data.OpEnergy.Account.API.V1.Hash as API
import qualified Data.OpEnergy.Account.API.V1.Account as API
import           Data.OpEnergy.Account.API.V2.AccountSecretResult
                 ( AccountSecretResult(..)
                 )

import           OpEnergy.Account.Server.V1.Config
import           OpEnergy.Account.Server.V1.Class
                 ( AppM, State(..), runLogging, profile)
import           OpEnergy.Account.Server.V1.AccountService
                 ( mgetPersonByAccountToken)
import           OpEnergy.Account.Server.V1.Person

import           OpEnergy.Error
                 ( eitherThrowJSON, runExceptPrefixT
                 , CallstackError, accountNotFound
                 )
import           OpEnergy.ExceptMaybe(exceptTMaybeT)


-- | V2 secret/regenerate endpoint: rotates the account secret and returns
-- the new one
regenerateSecretHandler
  :: API.AccountToken
  -> AppM AccountSecretResult
regenerateSecretHandler token =
    let name = "V2.regenerateSecretHandler"
    in profile name $ eitherThrowJSON
      ( runLogging . $(logError))
      $ regenerateSecret token

-- | business logic for V2 secret regeneration
regenerateSecret
  :: API.AccountToken
  -> AppM (Either CallstackError AccountSecretResult)
regenerateSecret token =
    let name = "V2.regenerateSecret"
    in profile name $ runExceptPrefixT name $ do
  State{ config = Config { configSalt = configSalt
                         , configAccountTokenEncryptionPrivateKey =
                             configAccountTokenEncryptionPrivateKey
                         }
       , accountDBPool = pool
       } <- lift ask
  (Entity personKey _) <- exceptTMaybeT accountNotFound
    $ mgetPersonByAccountToken token
  secret <- liftIO $! API.generateAccountSecret configSalt
  encryptedSecret <- liftIO
    $! encryptSecret configAccountTokenEncryptionPrivateKey secret
  let hashedSecret = API.hashSBS configSalt API.unAccountSecret secret
  liftIO $! flip runSqlPersistMPool pool $ do
    nowUTC <- liftIO getCurrentTime
    let now = utcTimeToPOSIXSeconds nowUTC
    update personKey
      [ PersonHashedSecret =. hashedSecret
      , PersonEncryptedSecret =. Just encryptedSecret
      , PersonLastUpdated =. now
      ]
  return $! AccountSecretResult secret
