{-- | V2 login-by-password handler.
 -}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE OverloadedStrings          #-}
module OpEnergy.Account.Server.V2.AccountService.LoginByPassword
  ( loginByPasswordHandler
  , loginByPassword
  ) where

import           Control.Monad (when)
import           Control.Monad.Trans.Reader (ask)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Logger(logError)
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Except (throwE)
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString.Lazy as LBS
import           Database.Persist.Postgresql
import qualified Crypto.BCrypt as BCrypt
import qualified Data.Aeson as Aeson
import qualified Web.ClientSession as ClientSession
import qualified Prometheus as P

import qualified Data.OpEnergy.Account.API.V1.Account as API
import           Data.OpEnergy.Account.API.V1.Password
                 ( Password(..), HashedPassword(..)
                 )
import           Data.OpEnergy.Account.API.V2.PasswordLoginRequest
                 ( PasswordLoginRequest(..)
                 )
import           Data.OpEnergy.Account.API.V2.LoginResult
                 ( LoginResult(..)
                 )

import           OpEnergy.Account.Server.V1.Config
import           OpEnergy.Account.Server.V1.Class
                 ( AppM, State(..), runLogging, profile)
import           OpEnergy.Account.Server.V1.Metrics(MetricsState(..))
import           OpEnergy.Account.Server.V1.AccountService
                 ( mgetPersonByDisplayName)
import           OpEnergy.Account.Server.V1.Person

import           OpEnergy.Error
                 ( eitherThrowJSON, runExceptPrefixT
                 , CallstackError, invalidCredentials, passwordNotSet
                 )
import           OpEnergy.ExceptMaybe(exceptTMaybeT)


-- | V2 login-by-password endpoint
loginByPasswordHandler
  :: PasswordLoginRequest
  -> AppM LoginResult
loginByPasswordHandler req =
    let name = "V2.loginByPasswordHandler"
    in profile name $ eitherThrowJSON
      ( runLogging . $(logError))
      $ loginByPassword req

-- | business logic for V2 login by password
loginByPassword
  :: PasswordLoginRequest
  -> AppM (Either CallstackError LoginResult)
loginByPassword (PasswordLoginRequest dn pw) =
    let name = "V2.loginByPassword"
    in profile name $ runExceptPrefixT name $ do
  State{ config = Config { configAccountTokenEncryptionPrivateKey =
                           configAccountTokenEncryptionPrivateKey
                         }
       , accountDBPool = pool
       , metrics = MetricsState { accountTokenEncrypt = accountTokenEncrypt
                                , accountUpdateLoginsCount = accountUpdateLoginsCount
                                }
       } <- lift ask
  -- use invalidCredentials for both unknown user and wrong password
  (Entity personKey person) <- exceptTMaybeT invalidCredentials
    $ mgetPersonByDisplayName dn
  (HashedPassword storedHash) <- exceptTMaybeT passwordNotSet
    $! return (personHashedPassword person)
  let valid = BCrypt.validatePassword
        (Text.encodeUtf8 storedHash)
        (Text.encodeUtf8 (unPassword pw))
  when (not valid) $ throwE invalidCredentials
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
    True -- password login implies a password is set
