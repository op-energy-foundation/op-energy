{-- | This module implements Account service in terms of OpEnergy.Account.API.V2.AccountV2API API
 -}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE OverloadedStrings          #-}
module OpEnergy.Account.Server.V2.AccountService
  ( login
  , setPassword
  , loginByPassword
  ) where

import           Servant (err400, err401, err500, NoContent(..))
import           Control.Monad.Trans.Reader (ask)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Logger(logError)
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString.Lazy as LBS

import qualified Data.Aeson as Aeson
import qualified Crypto.BCrypt as BCrypt
import qualified Web.ClientSession as ClientSession
import           Database.Persist.Postgresql
import qualified Prometheus as P


import           Data.OpEnergy.Account.API.V2
import qualified Data.OpEnergy.Account.API.V1.Hash as API
import qualified Data.OpEnergy.Account.API.V1.Account as API
import qualified Data.OpEnergy.Account.API.V1.Password as API
import           Data.OpEnergy.API.V1.Error

import           OpEnergy.Account.Server.V1.Config
import           OpEnergy.Account.Server.V1.Class ( AppM, State(..), runLogging)
import           OpEnergy.Account.Server.V1.Metrics(MetricsState(..))
import           OpEnergy.Account.Server.V1.AccountService
                 ( mgetPersonByHashedSecret
                 , mgetPersonByAccountToken
                 , mgetPersonByDisplayName
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

-- | see OpEnergy.Account.API.V2.AccountV2API for reference of 'password' API
-- call.
-- Authenticates by account token rather than by the current password, so that
-- a person who arrived here through their secret link -- which is the only
-- credential they have before this call succeeds -- is still able to set one.
setPassword :: API.AccountToken-> SetPasswordRequest-> AppM NoContent
setPassword token (SetPasswordRequest password) = do
  State{ accountDBPool = pool } <- ask
  mperson <- mgetPersonByAccountToken token
  case mperson of
    Nothing -> do
      let err = "ERROR: setPassword: failed to find user account with given token"
      runLogging $ $(logError) err
      throwJSON err401 err
    Just (Entity personKey _) -> do
      mhashed <- liftIO $! hashPassword password
      case mhashed of
        Nothing -> do
          -- bcrypt returns Nothing only when its policy is malformed, which
          -- would be a defect here rather than bad input from the caller
          let err = "ERROR: setPassword: failed to hash password"
          runLogging $ $(logError) err
          throwJSON err500 err
        Just hashed -> do
          liftIO $! flip runSqlPersistMPool pool $
            update personKey [ PersonHashedPassword =. Just hashed ]
          return NoContent

-- | see OpEnergy.Account.API.V2.AccountV2API for reference of 'login/password'
-- API call.
-- Returns the same LoginResult as the secret-based 'login' above, so that a
-- caller can treat either way of arriving at a session identically.
loginByPassword :: PasswordLoginRequest-> AppM LoginResult
loginByPassword (PasswordLoginRequest displayName password) = do
  State{ config = Config { configAccountTokenEncryptionPrivateKey =
                             configAccountTokenEncryptionPrivateKey
                         }
       , accountDBPool = pool
       , metrics = MetricsState { accountLogin = accountLogin
                                , accountTokenEncrypt = accountTokenEncrypt
                                , accountUpdateLoginsCount = accountUpdateLoginsCount
                                }
       } <- ask
  P.observeDuration accountLogin $ do
    mperson <- mgetPersonByDisplayName displayName
    case mperson of
      Nothing -> do
        -- deliberately the same error as a wrong password below: telling the
        -- caller which of the two was wrong would let them enumerate which
        -- display names exist
        let err = "ERROR: loginByPassword: invalid display name or password"
        runLogging $ $(logError) err
        throwJSON err400 err
      Just (Entity personKey person)
        | not (verifyPassword password (personHashedPassword person)) -> do
            let err = "ERROR: loginByPassword: invalid display name or password"
            runLogging $ $(logError) err
            throwJSON err400 err
        | otherwise -> do
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

-- | bcrypt digest of a plaintext password. Returns Nothing only if bcrypt's
-- own policy is malformed.
hashPassword :: API.Password-> IO (Maybe API.HashedPassword)
hashPassword password = do
  mhashed <- BCrypt.hashPasswordUsingPolicy BCrypt.slowerBcryptHashingPolicy
    $! Text.encodeUtf8 $! API.unPassword password
  return $! fmap (API.HashedPassword . Text.decodeUtf8) mhashed

-- | verifies a candidate password against a person's stored digest. A person
-- who has not set a password (Nothing) can never be authenticated this way --
-- they still have their account secret.
verifyPassword :: API.Password-> Maybe API.HashedPassword-> Bool
verifyPassword _ Nothing = False
verifyPassword candidate (Just hashed) =
  BCrypt.validatePassword
    (Text.encodeUtf8 $! API.unHashedPassword hashed)
    (Text.encodeUtf8 $! API.unPassword candidate)

