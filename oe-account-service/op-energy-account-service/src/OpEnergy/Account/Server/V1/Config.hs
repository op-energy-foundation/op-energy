{-# LANGUAGE OverloadedStrings #-}
module OpEnergy.Account.Server.V1.Config where

import           Data.Text (Text)
import qualified Data.Text.Encoding as Text
import           Data.Maybe
import qualified Data.ByteString.Char8 as BS
import qualified System.Environment as E
import           Data.OpEnergy.API.V1.Positive
import           Control.Monad.Catch
import           Control.Monad.Logger(LogLevel(..))

import           Data.Aeson(FromJSON, withText, withObject, (.:?), (.!=))
import           Data.Aeson.Types (Parser)
import qualified Data.Aeson as A
import           Web.ClientSession (Key)
import qualified Web.ClientSession as ClientSession
import qualified Data.ByteString.Base64 as Base64

instance MonadThrow Parser where
  throwM = fail . show

instance FromJSON LogLevel where
  parseJSON = withText "LogLevel" $ \v->
    pure $ case v of
      "Debug" -> LevelDebug
      "Info" -> LevelInfo
      "Warn" -> LevelWarn
      "Error" -> LevelError
      other -> LevelOther other

-- | Describes configurable options
data Config = Config
  { configDBPort :: Int
  , configDBHost:: Text
  , configDBUser :: Text
  , configDBName :: Text
  , configDBPassword :: Text
  , configDBConnectionPoolSize :: Positive Int
    -- ^ DB connection pool size
  , configSalt :: Text
    -- ^ this value is being used as a salt for secrets/token generation
  , configHTTPAPIPort :: Int
    -- ^ this port should be used to receive HTTP requests
  , configSchedulerPollRateSecs :: Positive Int
    -- ^ scheduler interval
  , configWebsocketKeepAliveSecs :: Positive Int
    -- ^ how many seconds to wait until ping packet will be sent
  , configLogLevelMin :: LogLevel
    -- ^ minimum log level to display
  , configPrometheusPort :: Positive Int
    -- ^ port which should be used by prometheus metrics
  , configCacheChunkSize :: Positive Int
    -- ^ defines size of chunk with which cache is grown
  , configAccountTokenEncryptionPrivateKey :: Key
    -- ^ secret key used to encrypt/decrypt AccountToken
  }
  deriving Show
instance FromJSON Config where
  parseJSON = withObject "Config" $ \v-> Config
    <$> ( v .:? "DB_PORT" .!= (configDBPort defaultConfig))
    <*> ( v .:? "DB_HOST" .!= (configDBHost defaultConfig))
    <*> ( v .:? "DB_USER" .!= (configDBUser defaultConfig))
    <*> ( v .:? "DB_NAME" .!= (configDBName defaultConfig))
    <*> ( v .:? "DB_PASSWORD" .!= (configDBPassword defaultConfig))
    <*> ( v .:? "DB_CONNECTION_POOL_SIZE" .!= (configDBConnectionPoolSize defaultConfig))
    <*> ( v .:? "SECRET_SALT" .!= (configSalt defaultConfig))
    <*> ( v .:? "API_HTTP_PORT" .!= (configHTTPAPIPort defaultConfig))
    <*> ( v .:? "SCHEDULER_POLL_RATE_SECS" .!= (configSchedulerPollRateSecs defaultConfig))
    <*> ( v .:? "WEBSOCKET_KEEP_ALIVE_SECS" .!= (configWebsocketKeepAliveSecs defaultConfig))
    <*> ( v .:? "LOG_LEVEL_MIN" .!= (configLogLevelMin defaultConfig))
    <*> ( v .:? "PROMETHEUS_PORT" .!= (configPrometheusPort defaultConfig))
    <*> ( v .:? "CACHE_CHUNK_SIZE" .!= (configCacheChunkSize defaultConfig))
    <*> ( v .:? "ACCOUNT_TOKEN_ENCRYPTION_PRIVATE_KEY" .!= (configAccountTokenEncryptionPrivateKey defaultConfig))

-- need to get Key from json, which represented as base64-encoded string
instance FromJSON Key where
  parseJSON = withText "Key" $ \v-> case Base64.decode $! Text.encodeUtf8 v of
    Right some -> case ClientSession.initKey some of
      Right key -> return key
      Left err -> error ("ERROR: FromJSON Key/ClientSession.initKey: " ++ err)
    Left err -> error ("ERROR: FromJSON Key: " ++ err)

defaultConfig:: Config
defaultConfig = Config
  { configDBPort = 5432
  , configDBHost = "localhost"
  , configDBUser = "openergy"
  , configDBName = "openergy"
  , configDBPassword = ""
  , configDBConnectionPoolSize = 32
  , configSalt = ""
  , configHTTPAPIPort = 8899
  , configSchedulerPollRateSecs = verifyPositive 1
  , configWebsocketKeepAliveSecs = 10
  , configLogLevelMin = LevelWarn
  , configPrometheusPort = 7899
  , configCacheChunkSize = 50000
  , configAccountTokenEncryptionPrivateKey = error "defaultConfig: you are missing ACCOUNT_TOKEN_ENCRYPTION_PRIVATE_KEY from config. Please generate it with \"dd if=/dev/urandom bs=1 count=96 2>/dev/null | base64\" command"
  }

getConfigFromEnvironment :: IO Config
getConfigFromEnvironment = do
  configFilePath <- E.lookupEnv "OPENERGY_ACCOUNT_SERVICE_CONFIG_FILE" >>= pure . fromMaybe "./op-energy-account-service-config.json"
  configStr <- BS.readFile configFilePath
  case A.eitherDecodeStrict configStr of
    Left some -> error $ configFilePath ++ " is not a valid config: " ++ some
    Right config -> return config
