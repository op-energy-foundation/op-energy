{-- | V2 login handler: authenticates by account secret.
 -}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE OverloadedStrings          #-}
module OpEnergy.Account.Server.V2.AccountService.Login
  ( loginHandler
  , login
  ) where

import           Control.Monad.Trans.Reader (ask)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Logger(logError)
import           Control.Monad.Trans (lift)
import           Data.Maybe (isJust)
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString.Lazy as LBS

import           Database.Persist.Postgresql
import qualified Data.Aeson as Aeson
import qualified Web.ClientSession as ClientSession
import qualified Prometheus as P

import qualified Data.OpEnergy.Account.API.V1.Hash as API
import qualified Data.OpEnergy.Account.API.V1.Account as API
import           Data.OpEnergy.Account.API.V2.LoginResult
                 ( LoginResult(..)
                 )

import           OpEnergy.Account.Server.V1.Config
import           OpEnergy.Account.Server.V1.Class
                 ( AppM, State(..), runLogging, profile)
import           OpEnergy.Account.Server.V1.Metrics(MetricsState(..))
import           OpEnergy.Account.Server.V1.AccountService
                 ( mgetPersonByHashedSecret)
import           OpEnergy.Account.Server.V1.Person

import           OpEnergy.Error
                 ( eitherThrowJSON, runExceptPrefixT
                 , CallstackError, authenticationFailure
                 )
import           OpEnergy.ExceptMaybe(exceptTMaybeT)


-- | V2 login endpoint
loginHandler
  :: API.AccountSecret
  -> AppM LoginResult
loginHandler secret =
    let name = "V2.loginHandler"
    in profile name $ eitherThrowJSON
      ( runLogging . $(logError))
      $ login secret

-- | business logic for V2 login
login
  :: API.AccountSecret
  -> AppM (Either CallstackError LoginResult)
login secret =
    let name = "V2.login"
    in profile name $ runExceptPrefixT name $ do
  State{ config = Config { configSalt = configSalt
                         , configAccountTokenEncryptionPrivateKey =
                           configAccountTokenEncryptionPrivateKey
                         }
       , accountDBPool = pool
       , metrics = MetricsState { accountTokenEncrypt = accountTokenEncrypt
                                , accountUpdateLoginsCount = accountUpdateLoginsCount
                                }
       } <- lift ask
  let hashedSecret = API.hashSBS configSalt API.unAccountSecret secret
  (Entity personKey person) <- exceptTMaybeT authenticationFailure
    $ mgetPersonByHashedSecret hashedSecret
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
    (personDisplayName person)
    (isJust (personHashedPassword person))
