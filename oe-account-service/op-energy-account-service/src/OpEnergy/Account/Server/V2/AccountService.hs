{-- | This module implements Account service in terms of OpEnergy.Account.API.V2.AccountV2API API
 -}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE OverloadedStrings          #-}
module OpEnergy.Account.Server.V2.AccountService
  ( login
  , whoami
  , deductBalance
  , creditBalance
  ) where

import           Servant (err400, err401)
import           Control.Monad.Trans.Reader (ask)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Logger(logError)
import           Data.Int(Int64)
import           Data.Text(Text)
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

-- | see OpEnergy.Account.API.V2.AccountV2API for reference of 'whoami' API
-- call.
-- Thin wrapper over V1's own mgetPersonByAccountToken (the same lookup V1's
-- postDisplayName already relies on to authenticate a caller) -- exists so
-- that other op-energy services have a way to resolve a token at all,
-- without reaching into this service's DB or duplicating its token-decrypt
-- logic themselves.
whoami :: API.AccountToken -> AppM WhoAmIResult
whoami token = do
  mperson <- mgetPersonByAccountToken token
  case mperson of
    Nothing -> do
      let err = "ERROR: whoami: failed to find user account with given token"
      runLogging $ $(logError) err
      throwJSON err401 err
    Just (Entity _ person) -> return $!
      WhoAmIResult
        (apiModelUUIDPerson $ personUuid person)
        (personDisplayName person)
        (personBalance person)

-- | shared first step of 'deductBalance'/'creditBalance': rejects the call
-- outright if the caller didn't present the configured shared secret.
-- Never called on anything a browser client can reach -- see
-- Data.OpEnergy.Account.API.V2's own header description.
checkInternalServiceSecret :: Text -> AppM ()
checkInternalServiceSecret secret = do
  State{ config = Config{ configInternalServiceSharedSecret = expected } } <- ask
  if secret /= expected
    then do
      let err = "ERROR: internal balance call: invalid X-Internal-Service-Secret" :: Text
      runLogging $ $(logError) err
      throwJSON err401 err
    else return ()

-- | see OpEnergy.Account.API.V2.AccountV2API for reference of the
-- 'internal/balance/deduct' API call.
-- Atomic conditional decrement ("balance >= amountSats"), not a
-- read-then-write -- so two concurrent deducts against the same account
-- can't both pass a balance check that's gone stale by the time either
-- writes. This is oe-offer-service's replacement for what would otherwise
-- be a local Wallet table it doesn't have: balance lives here, on the
-- account this service already owns, not duplicated into every caller.
deductBalance :: Text -> BalanceAdjustRequest -> AppM BalanceAdjustResult
deductBalance secret (BalanceAdjustRequest personUUIDV amountSats) = do
  checkInternalServiceSecret secret
  State{ accountDBPool = pool } <- ask
  let modelUUID = modelApiUUIDPerson personUUIDV
  eresult <- liftIO $ flip runSqlPersistMPool pool $ do
    mperson <- selectFirst [ PersonUuid ==. modelUUID ] []
    case mperson of
      Nothing -> return $! Left ("deductBalance: unknown personUUID" :: Text)
      Just (Entity key person) -> do
        deducted <- updateWhereCount
          [ PersonId ==. key, PersonBalance >=. amountSats ]
          [ PersonBalance -=. amountSats ]
        if deducted /= (1 :: Int64)
          then return $! Left "deductBalance: insufficient balance"
          else return $! Right (personBalance person - amountSats)
  case eresult of
    Left err -> do
      runLogging $ $(logError) err
      throwJSON err400 err
    Right newBalance -> return $! BalanceAdjustResult newBalance

-- | see OpEnergy.Account.API.V2.AccountV2API for reference of the
-- 'internal/balance/credit' API call.
-- Unconditional increment -- crediting a refund can never fail on the
-- balance itself, only on the personUUID not being known at all.
creditBalance :: Text -> BalanceAdjustRequest -> AppM BalanceAdjustResult
creditBalance secret (BalanceAdjustRequest personUUIDV amountSats) = do
  checkInternalServiceSecret secret
  State{ accountDBPool = pool } <- ask
  let modelUUID = modelApiUUIDPerson personUUIDV
  eresult <- liftIO $ flip runSqlPersistMPool pool $ do
    mperson <- selectFirst [ PersonUuid ==. modelUUID ] []
    case mperson of
      Nothing -> return $! Left ("creditBalance: unknown personUUID" :: Text)
      Just (Entity key person) -> do
        update key [ PersonBalance +=. amountSats ]
        return $! Right (personBalance person + amountSats)
  case eresult of
    Left err -> do
      runLogging $ $(logError) err
      throwJSON err400 err
    Right newBalance -> return $! BalanceAdjustResult newBalance

