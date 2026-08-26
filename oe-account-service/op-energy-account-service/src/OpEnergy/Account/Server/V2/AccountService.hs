{-- | This module implements Account service in terms of OpEnergy.Account.API.V2.AccountV2API API
 -}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE OverloadedStrings          #-}
module OpEnergy.Account.Server.V2.AccountService
  ( login
  , setPassword
  , loginByPassword
  , register
  , getMe
  , postDisplayName
  , displayNameExists
  , getSecret
  , regenerateSecret
  ) where

import           Servant (err400, err401, err404, err500, NoContent(..))
import           Control.Monad.Trans.Reader (ask)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Logger(logError)
import           Data.Maybe(isJust)
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString.Lazy as LBS

import qualified Crypto.BCrypt as BCrypt
import qualified Data.Aeson as Aeson
import qualified Web.ClientSession as ClientSession
import           Database.Persist.Postgresql
import qualified Prometheus as P


import           Data.OpEnergy.Account.API.V2
import qualified Data.OpEnergy.Account.API.V1 as V1API
import qualified Data.OpEnergy.Account.API.V1.Hash as API
import qualified Data.OpEnergy.Account.API.V1.Account as API
import qualified Data.OpEnergy.Account.API.V1.Password as API
import           Data.OpEnergy.API.V1.Error

import           OpEnergy.Account.Server.V1.Config
import           OpEnergy.Account.Server.V1.Class ( AppM, State(..), runLogging)
import           OpEnergy.Account.Server.V1.Metrics(MetricsState(..))
import qualified OpEnergy.Account.Server.V1.AccountService
                 as V1 ( register
                       , postDisplayName
                       , mgetPersonByHashedSecret
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
    mperson <- V1.mgetPersonByHashedSecret hashedSecret
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
          (API.verifyAccountToken $! Text.decodeUtf8 token)
          (apiModelUUIDPerson $ personUuid person)

-- | see OpEnergy.Account.API.V2.AccountV2API for reference of 'password' API
-- call.
-- Authenticates by account token rather than by the current password, so that
-- a person who arrived here through their secret link -- which is the only
-- credential they have before this call succeeds -- is still able to set one.
setPassword :: API.AccountToken-> SetPasswordRequest-> AppM NoContent
setPassword token (SetPasswordRequest password) = do
  State{ accountDBPool = pool } <- ask
  mperson <- V1.mgetPersonByAccountToken token
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
    mperson <- V1.mgetPersonByDisplayName displayName
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
              (API.verifyAccountToken $! Text.decodeUtf8 token)
              (apiModelUUIDPerson $ personUuid person)

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

-- | see OpEnergy.Account.API.V2.AccountV2API for reference of 'register' API
-- call.
-- Registration itself is V1's: this call exists to also return the display
-- name that was assigned, which the frontend displays as soon as a visitor
-- arrives and would otherwise have to ask for in a second call.
register :: AppM RegisterResultV2
register = do
  result <- V1.register
  let token = V1API.accountToken (result :: V1API.RegisterResult)
  mperson <- V1.mgetPersonByAccountToken token
  case mperson of
    Nothing -> do
      -- unreachable: the token was just minted for a row that was just
      -- inserted, so failing to find it means the two disagree
      let err = "ERROR: register: freshly registered account cannot be found"
      runLogging $ $(logError) err
      throwJSON err400 err
    Just (Entity _ person) -> return $! RegisterResultV2
      (V1API.accountSecret (result :: V1API.RegisterResult))
      token
      (V1API.personUUID (result :: V1API.RegisterResult))
      (personDisplayName person)

-- | see OpEnergy.Account.API.V2.AccountV2API for reference of 'me' API call.
-- Answers "who is this token", which is what a client needs in order to
-- restore a session from the token it has stored.
getMe :: API.AccountToken-> AppM AccountInfo
getMe token = do
  mperson <- V1.mgetPersonByAccountToken token
  case mperson of
    Nothing -> do
      let err = "ERROR: getMe: failed to find user account with given token"
      runLogging $ $(logError) err
      throwJSON err401 err
    Just (Entity _ person) -> return $! AccountInfo
      (personDisplayName person)
      (isJust $! personHashedPassword person)

-- | see OpEnergy.Account.API.V2.AccountV2API for reference of 'displayname'
-- API call.
-- The rename itself is V1's, including its uniqueness check; this call returns
-- the resulting account state rather than nothing, so a client does not have
-- to assume the value it just sent is now in effect.
postDisplayName :: API.AccountToken-> API.DisplayName-> AppM AccountInfo
postDisplayName token displayName = do
  _ <- V1.postDisplayName $! V1API.PostUserDisplayNameRequest token displayName
  getMe token

-- | see OpEnergy.Account.API.V2.AccountV2API for reference of
-- 'displayname/exists' API call.
-- Deliberately returns only whether the name is taken: it is unauthenticated,
-- so it must not become a way to read anything else about an account.
displayNameExists :: API.DisplayName-> AppM DisplayNameExistsResult
displayNameExists displayName = do
  mperson <- V1.mgetPersonByDisplayName displayName
  return $! DisplayNameExistsResult (isJust mperson)

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
    mperson <- V1.mgetPersonByAccountToken token
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
    mperson <- V1.mgetPersonByAccountToken token
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

