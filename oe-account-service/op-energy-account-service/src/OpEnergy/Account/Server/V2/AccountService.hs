{-- | This module implements Account service in terms of OpEnergy.Account.API.V2.AccountV2API API
 -}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE OverloadedStrings          #-}
module OpEnergy.Account.Server.V2.AccountService
  ( login
  , getSecret
  , regenerateSecret
  ) where

import           Servant (err400, err401, err404, err500)
import           Control.Monad.Trans.Reader (ask)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Logger(logError)
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString.Lazy as LBS

import qualified Data.Aeson as Aeson
import qualified Web.ClientSession as ClientSession
import           Database.Persist.Postgresql
import qualified Prometheus as P


import           Data.OpEnergy.Account.API.V2
import qualified Data.OpEnergy.Account.API.V1.Hash as API
import qualified Data.OpEnergy.Account.API.V1.Account as API
import           Data.OpEnergy.API.V1.Error

import           OpEnergy.Account.Server.V1.Config
import           OpEnergy.Account.Server.V1.Class ( AppM, State(..), runLogging)
import           OpEnergy.Account.Server.V1.Metrics(MetricsState(..))
import           OpEnergy.Account.Server.V1.AccountService
                 ( mgetPersonByHashedSecret
                 , mgetPersonByAccountToken
                 )
import           OpEnergy.Account.Server.V1.Person


-- | see OpEnergy.Account.API.V2.AccountV2API for reference of 'login' API call
-- 3 * O(ln n)
login :: API.AccountSecret -> AppM LoginResult
login secret = do
  State{ config = Config { configSalt = configSalt
                         , configAccountTokenEncryptionPrivateKey =
                           configAccountTokenEncryptionPrivateKey
                         }
       , accountDBPool = pool
       , metrics = MetricsState { accountLogin = accountLogin
                                , accountTokenEncrypt = accountTokenEncrypt
                                , accountUpdateLoginsCount = accountUpdateLoginsCount
                                }
       } <- ask
  P.observeDuration accountLogin $ do
    let hashedSecret = API.hashSBS configSalt API.unAccountSecret secret
    mperson <- mgetPersonByHashedSecret hashedSecret
    case mperson of
      Nothing -> do
        let err = "ERROR: login: failed to find user account with given secret"
        runLogging $ $(logError) err
        throwJSON err400 err
      Just (Entity personKey person) -> do
        -- increase loginsCount returning new value
        loginsCount <- liftIO $! P.observeDuration accountUpdateLoginsCount
          $ flip runSqlPersistMPool pool $ do
            update personKey [ PersonLoginsCount =. (personLoginsCount person + 1) ]
            return (personLoginsCount person + 1)
        token <- liftIO $ P.observeDuration accountTokenEncrypt
          $! ClientSession.encryptIO configAccountTokenEncryptionPrivateKey
          $! LBS.toStrict $! Aeson.encode (personUuid person, loginsCount)
        return $! LoginResult
          { accountToken = API.verifyAccountToken $! Text.decodeUtf8 token
          , personUUID = apiModelUUIDPerson $ personUuid person
          }

-- | see OpEnergy.Account.API.V2.AccountV2API for reference of 'secret' API
-- call.
-- Decrypts the stored secret so that the frontend can show the secret link
-- back to the person it belongs to. Only the encrypted copy can serve this:
-- personHashedSecret is one-way and exists to be looked up by, not read back.
getSecret :: API.AccountToken-> AppM AccountSecretResult
getSecret token = do
  State{ config = Config { configAccountTokenEncryptionPrivateKey =
                             configAccountTokenEncryptionPrivateKey
                         }
       , metrics = MetricsState { accountGetSecret = accountGetSecret }
       } <- ask
  P.observeDuration accountGetSecret $ do
    mperson <- mgetPersonByAccountToken token
    case mperson of
      Nothing -> do
        let err = "ERROR: getSecret: failed to find user account with given token"
        runLogging $ $(logError) err
        throwJSON err401 err
      Just (Entity _ person) -> case personEncryptedSecret person of
        Nothing -> do
          -- registered before the encrypted copy existed: the secret survives
          -- only as a hash, so it can never be displayed again. Regenerating
          -- is the only way for such a person to get a secret link back
          let err = "ERROR: getSecret: this account has no recoverable secret, regenerate it instead"
          runLogging $ $(logError) err
          throwJSON err404 err
        Just encryptedSecret ->
          case decryptSecret configAccountTokenEncryptionPrivateKey encryptedSecret of
            Nothing -> do
              -- the stored ciphertext does not decrypt with the current key,
              -- which means the key changed after this row was written
              let err = "ERROR: getSecret: failed to decrypt stored secret"
              runLogging $ $(logError) err
              throwJSON err500 err
            Just secret -> return $! AccountSecretResult
              { accountSecret = secret
              }


-- | see OpEnergy.Account.API.V2.AccountV2API for reference of
-- 'secret/regenerate' API call.
-- Replaces the person's secret with a freshly generated one and returns it.
-- Both stored forms are replaced together: the hash, which login looks the
-- person up by, and the encrypted copy, which 'getSecret' reads back. The
-- previous secret stops matching either, which is what makes a shared or lost
-- secret link revocable.
-- The account token is deliberately left alone, so the caller performing the
-- rotation is not logged out by it.
regenerateSecret :: API.AccountToken-> AppM AccountSecretResult
regenerateSecret token = do
  State{ config = Config { configSalt = configSalt
                         , configAccountTokenEncryptionPrivateKey =
                             configAccountTokenEncryptionPrivateKey
                         }
       , accountDBPool = pool
       , metrics = MetricsState { accountRegenerateSecret = accountRegenerateSecret }
       } <- ask
  P.observeDuration accountRegenerateSecret $ do
    mperson <- mgetPersonByAccountToken token
    case mperson of
      Nothing -> do
        let err = "ERROR: regenerateSecret: failed to find user account with given token"
        runLogging $ $(logError) err
        throwJSON err401 err
      Just (Entity personKey _) -> do
        secret <- liftIO $! API.generateAccountSecret configSalt
        encryptedSecret <- liftIO
          $! encryptSecret configAccountTokenEncryptionPrivateKey secret
        let hashedSecret = API.hashSBS configSalt API.unAccountSecret secret
        liftIO $! flip runSqlPersistMPool pool $
          update personKey
            [ PersonHashedSecret =. hashedSecret
            , PersonEncryptedSecret =. Just encryptedSecret
            ]
        return $! AccountSecretResult { accountSecret = secret }

