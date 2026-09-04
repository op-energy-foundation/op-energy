{-# LANGUAGE OverloadedStrings #-}
module OpEnergy.Offer.Server.V1.Config where

import           Data.Text (Text)
import           Data.Maybe
import qualified Data.ByteString.Char8 as BS
import qualified System.Environment as E
import           Data.OpEnergy.API.V1.Positive
import           Control.Monad.Catch
import           Control.Monad.Logger(LogLevel(..))

import           Data.Aeson(FromJSON, withText, withObject, (.:?), (.!=))
import           Data.Aeson.Types (Parser)
import qualified Data.Aeson as A
import           Servant.Client (BaseUrl(..), showBaseUrl, parseBaseUrl, Scheme(..))

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
  , configHTTPAPIPort :: Int
    -- ^ this port should be used to receive HTTP requests
  , configSchedulerPollRateSecs :: Positive Int
    -- ^ scheduler interval
  , configLogLevelMin :: LogLevel
    -- ^ minimum log level to display
  , configPrometheusPort :: Positive Int
    -- ^ port which should be used by prometheus metrics
  , configAccountServiceURL :: BaseUrl
    -- ^ base URL of oe-account-service's HTTP API
  , configInternalServiceSharedSecret :: Text
    -- ^ sent as the X-Internal-Service-Secret header on every
    -- internal/balance/{deduct,credit} call
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
    <*> ( v .:? "API_HTTP_PORT" .!= (configHTTPAPIPort defaultConfig))
    <*> ( v .:? "SCHEDULER_POLL_RATE_SECS" .!= (configSchedulerPollRateSecs defaultConfig))
    <*> ( v .:? "LOG_LEVEL_MIN" .!= (configLogLevelMin defaultConfig))
    <*> ( v .:? "PROMETHEUS_PORT" .!= (configPrometheusPort defaultConfig))
    <*> ((v .:? "ACCOUNT_SERVICE_API_URL" .!= (showBaseUrl $ configAccountServiceURL defaultConfig)) >>= parseBaseUrl)
    <*> ( v .:? "INTERNAL_SERVICE_SHARED_SECRET" .!= (configInternalServiceSharedSecret defaultConfig))

defaultConfig:: Config
defaultConfig = Config
  { configDBPort = 5432
  , configDBHost = "localhost"
  , configDBUser = "openergy"
  , configDBName = "openergyoffer"
  , configDBPassword = ""
  , configDBConnectionPoolSize = 32
  , configHTTPAPIPort = 8909
  , configSchedulerPollRateSecs = verifyPositive 10
  , configLogLevelMin = LevelWarn
  , configPrometheusPort = 7909
  , configAccountServiceURL = BaseUrl Http "127.0.0.1" 8899 ""
  , configInternalServiceSharedSecret = error "defaultConfig: you are missing INTERNAL_SERVICE_SHARED_SECRET from config -- must match oe-account-service's own value. Generate with \"dd if=/dev/urandom bs=1 count=32 2>/dev/null | base64 -w 0\" command"
  }

getConfigFromEnvironment :: IO Config
getConfigFromEnvironment = do
  configFilePath <- E.lookupEnv "OPENERGY_OFFER_SERVICE_CONFIG_FILE" >>= pure . fromMaybe "./op-energy-offer-service-config.json"
  configStr <- BS.readFile configFilePath
  case A.eitherDecodeStrict configStr of
    Left some -> error $ configFilePath ++ " is not a valid config: " ++ some
    Right config -> return config
