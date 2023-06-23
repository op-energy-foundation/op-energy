{-# LANGUAGE OverloadedStrings          #-}
module OpEnergy.Account.Server.V1.AccountService where

-- import           Servant (err404, throwError)
import           Control.Monad.Trans.Reader (ask)
import           Control.Monad.IO.Class (liftIO, MonadIO)
-- import qualified Data.Text.IO as Text
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Short as BS
import           Data.Time.Clock(getCurrentTime)
import           Data.Time.Clock.POSIX(utcTimeToPOSIXSeconds)

import qualified Data.Aeson as Aeson
import qualified Web.ClientSession as ClientSession
import           Database.Persist.Postgresql
-- import           Data.Text.Show (tshow)
import qualified Prometheus as P


import           Data.OpEnergy.Account.API.V1
import           Data.OpEnergy.Account.API.V1.Hash
import           Data.OpEnergy.Account.API.V1.Account
import           Data.OpEnergy.Account.API.V1.UUID
import           OpEnergy.Account.Server.V1.Config
import           OpEnergy.Account.Server.V1.Class (AppT, AppM, State(..))
import           OpEnergy.Account.Server.V1.Metrics(MetricsState(..))


register :: MonadIO m => AppT m RegisterResult
register = do
  State{ config = Config { configSalt = configSalt
                         , configAccountTokenEncryptionPrivateKey = configAccountTokenEncryptionPrivateKey
                         }
       , accountDBPool = pool
       , metrics = MetricsState { accountInsert = accountInsert }
       } <- ask
  nowUTC <- liftIO getCurrentTime
  let now = utcTimeToPOSIXSeconds nowUTC
  secret <- liftIO $ generateAccountSecret configSalt
  let hashedSecret = hashSBS configSalt unAccountSecret secret
      hashedToken = hashSBS configSalt unHashed hashedSecret
  -- insert record into cache
  uuid <- liftIO generateRandomUUID
  let UUID rawUUID = uuid
      userNameHash = verifyDisplayName $! "user" <> (Text.decodeUtf8 $! BS.take 6 $! BS.fromShort rawUUID)
      person = Person
        { personCreationTime = now
        , personUuid = uuid
        , personLastSeenTime = Nothing
        , personLastUpdated = now
        , personEmail = Nothing
        , personDisplayName = userNameHash
        , personTokenHashed = hashedToken
        }
  -- insert record into DB
  _ <- liftIO $! P.observeDuration accountInsert $ flip runSqlPersistMPool pool $ insert $! person
  token <- liftIO $ ClientSession.encryptIO configAccountTokenEncryptionPrivateKey $! LBS.toStrict $! Aeson.encode (now, hashedSecret)

  return $! RegisterResult
    { accountSecret = secret
    , accountToken = verifyAccountToken $! Text.decodeUtf8 token
    }
  
login :: AccountSecret -> AppM AccountToken
login secret = do
  State{ config = Config { configSalt = configSalt
                         , configAccountTokenEncryptionPrivateKey = configAccountTokenEncryptionPrivateKey
                         }
       , accountDBPool = pool
       , metrics = MetricsState { accountLogin = accountLogin }
       } <- ask
  nowUTC <- liftIO getCurrentTime
  let now = utcTimeToPOSIXSeconds nowUTC
      hashedSecret = hashSBS configSalt unAccountSecret secret
      hashedToken = hashSBS configSalt unHashed hashedSecret
  -- insert record into cache
  uuid <- liftIO generateRandomUUID
  let UUID rawUUID = uuid
      userNameHash = verifyDisplayName $! "user" <> (Text.decodeUtf8 $! BS.take 6 $! BS.fromShort rawUUID)
      person = Person
        { personCreationTime = now
        , personUuid = uuid
        , personLastSeenTime = Nothing
        , personLastUpdated = now
        , personEmail = Nothing
        , personDisplayName = userNameHash
        , personTokenHashed = hashedToken
        }
  -- insert record into DB
  _ <- liftIO $! P.observeDuration accountInsert $ flip runSqlPersistMPool pool $ insert $! person
  token <- liftIO $ ClientSession.encryptIO configAccountTokenEncryptionPrivateKey $! LBS.toStrict $! Aeson.encode (now, hashedSecret)

  return $! RegisterResult
    { accountSecret = secret
    , accountToken = verifyAccountToken $! Text.decodeUtf8 token
    }

-- | performs read from DB in order to set State.currentHeightTip
loadDBState :: AppT IO ()
loadDBState = undefined
