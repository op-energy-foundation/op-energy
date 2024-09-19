{-- | This module implements Account service in terms of OpEnergy.Account.API.V1.AccountV1API API, which you may
 - be insteresting to check for API description.
 -
 - Implementation details are defined in op-energy/README.md
 -
 - TODO: we may consider:
 - 1. using some random value of say [ 0; 10000] as default value for loginsCount instead of 0;
 - 2. increasing loginsCount not by 1, but by random value [1;10000].
 - This way we can increase an entropy in case of worries of lacking of randomness in token. This will reduce the time
 - to overflow the loginsCount accumulator, but it is expected to happen at some point anyway...
 -}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE OverloadedStrings          #-}
module OpEnergy.Account.Server.V2.AccountService
  ( login
  ) where

import           Servant (err400)
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
import           Data.OpEnergy.Account.API.V1.Hash
import           Data.OpEnergy.Account.API.V1.Account
import           OpEnergy.Account.Server.V1.Config
import           OpEnergy.Account.Server.V1.Class ( AppM, State(..), runLogging)
import           OpEnergy.Account.Server.V1.Metrics(MetricsState(..))
import           Data.OpEnergy.API.V1.Error
import           OpEnergy.Account.Server.V1.AccountService(mgetPersonByHashedSecret)


-- | see OpEnergy.Account.API.V2.AccountV1API for reference of 'login' API call
-- 3 * O(ln n)
login :: AccountSecret -> AppM LoginResult
login secret = do
  State{ config = Config { configSalt = configSalt
                         , configAccountTokenEncryptionPrivateKey = configAccountTokenEncryptionPrivateKey
                         }
       , accountDBPool = pool
       , metrics = MetricsState { accountLogin = accountLogin
                                , accountTokenEncrypt = accountTokenEncrypt
                                , accountUpdateLoginsCount = accountUpdateLoginsCount
                                }
       } <- ask
  P.observeDuration accountLogin $ do
    let hashedSecret = hashSBS configSalt unAccountSecret secret
    mperson <- mgetPersonByHashedSecret hashedSecret
    case mperson of
      Nothing -> do
        let err = "ERROR: login: failed to find user account with given secret"
        runLogging $ $(logError) err
        throwJSON err400 err
      Just (Entity personKey person) -> do
        -- increase loginsCount returning new value
        loginsCount <- liftIO $! P.observeDuration accountUpdateLoginsCount $ flip runSqlPersistMPool pool $ do
          update personKey [ PersonLoginsCount =. (personLoginsCount person + 1) ]
          return (personLoginsCount person + 1)
        token <- liftIO $ P.observeDuration accountTokenEncrypt $! ClientSession.encryptIO configAccountTokenEncryptionPrivateKey $! LBS.toStrict $! Aeson.encode (personUuid person, loginsCount)
        return $! LoginResult
          { accountToken = verifyAccountToken $! Text.decodeUtf8 token
          , personUUID = personUuid person
          }

