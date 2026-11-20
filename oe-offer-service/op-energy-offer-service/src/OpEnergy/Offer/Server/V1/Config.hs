{-# LANGUAGE OverloadedStrings #-}
module OpEnergy.Offer.Server.V1.Config where

import           Data.Text (Text)
import           Data.Maybe
import           Data.Int (Int64)
import           Data.Word (Word64)
import           Control.Monad (when)
import qualified Data.List as List
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

-- | the telegram bot's token.
--
-- A newtype with a Show of its own, rather than a Text field, because
-- Config derives Show and this token is the whole of the bot's authority:
-- anything, which prints a Config -- a debug line, an exception, a dump of
-- the parsed settings -- would otherwise hand it out.
newtype TelegramBotToken = TelegramBotToken
  { unTelegramBotToken :: Text
  }
  deriving Eq
instance Show TelegramBotToken where
  show _ = "TelegramBotToken <redacted>"
instance FromJSON TelegramBotToken where
  parseJSON = withText "TelegramBotToken" $ pure . TelegramBotToken

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
  , configBlockspanWebsocketURL :: BaseUrl
    -- ^ blockspan service's websocket, used to follow the chain tip
  , configBlockspanURL :: BaseUrl
    -- ^ base URL of blockspan service's HTTP API, used to get the
    -- mediantime of a contract's target block during settlement
  , configPlatformFeeSats :: Word64
    -- ^ fee deducted from the pot of every settled contract
  , configFreeSatsPairMatchLimit :: Word64
    -- ^ how many contracts two accounts may share within
    -- 'configFreeSatsPairMatchWindowSecs' when either side is staking free
    -- sats. A farm of accounts, which were all given free sats, turns them
    -- into money by taking both sides of its own contracts, which means the
    -- same two accounts meeting again and again. 0 switches the check off
  , configFreeSatsPairMatchWindowSecs :: Word64
    -- ^ how far back 'configFreeSatsPairMatchLimit' counts the contracts
    -- two accounts have already shared
  , configTelegramBotToken :: Maybe TelegramBotToken
    -- ^ the token of the bot, which mirrors the trade chat. Nothing leaves
    -- the mirror switched off: the feed is served, empty, and no thread
    -- follows any group. A deployment without a telegram group is a
    -- deployment, which has no token to give, so this is left out of the
    -- defaults rather than failing the service
  , configTelegramChatId :: Maybe Int64
    -- ^ the group the bot follows. Negative, as telegram's group ids are.
    -- Nothing switches the mirror off, as a missing token does
  , configTelegramPollTimeoutSecs :: Positive Int
    -- ^ how long telegram is asked to hold a poll open while it waits for
    -- a message. It answers at once when there is one, so this is what
    -- makes the mirror immediate rather than a poll rate
  , configTelegramAPIURL :: BaseUrl
    -- ^ telegram's bot API. Configurable so a test can point the mirror at
    -- a stub rather than at telegram
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
    <*> ((v .:? "BLOCKSPAN_WS_URL" .!= (showBaseUrl $ configBlockspanWebsocketURL defaultConfig)) >>= parseWebsocketUrl)
    <*> ((v .:? "BLOCKSPAN_API_URL" .!= (showBaseUrl $ configBlockspanURL defaultConfig)) >>= parseBaseUrl)
    <*> ( v .:? "PLATFORM_FEE_SATS" .!= (configPlatformFeeSats defaultConfig))
    <*> ( v .:? "FREE_SATS_PAIR_MATCH_LIMIT"
          .!= (configFreeSatsPairMatchLimit defaultConfig))
    <*> ( v .:? "FREE_SATS_PAIR_MATCH_WINDOW_SECS"
          .!= (configFreeSatsPairMatchWindowSecs defaultConfig))
    <*> ( v .:? "TELEGRAM_BOT_TOKEN")
    <*> ( v .:? "TELEGRAM_CHAT_ID")
    <*> ( v .:? "TELEGRAM_POLL_TIMEOUT_SECS"
          .!= (configTelegramPollTimeoutSecs defaultConfig))
    <*> ((v .:? "TELEGRAM_API_URL"
          .!= (showBaseUrl $ configTelegramAPIURL defaultConfig)) >>= parseBaseUrl)

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
  , configBlockspanWebsocketURL = BaseUrl Http "127.0.0.1" 8999 "/api/v1/ws"
  , configBlockspanURL = BaseUrl Http "127.0.0.1" 8999 ""
  , configPlatformFeeSats = 1000
  , configFreeSatsPairMatchLimit = 3
  , configFreeSatsPairMatchWindowSecs = 604800 -- a week
  , configTelegramBotToken = Nothing
  , configTelegramChatId = Nothing
  , configTelegramPollTimeoutSecs = verifyPositive 25
  , configTelegramAPIURL = BaseUrl Https "api.telegram.org" 443 ""
  }

-- | parses websocket URL. The websocket client does not support TLS, so
-- only @ws://@ and @http://@ (for consistency with the rest of the URLs in
-- this config) are accepted; @wss://@ and @https://@ are rejected.
--
-- Example: "ws://127.0.0.1:8999/api/v1/ws"
parseWebsocketUrl :: (MonadThrow m, MonadFail m) => String -> m BaseUrl
parseWebsocketUrl url = do
  burl <- parseBaseUrl $! maybe url ("http://" <>) (List.stripPrefix "ws://" url)
  when (baseUrlScheme burl /= Http) $
    fail ("parseWebsocketUrl: TLS is not supported, use ws:// instead of " <> url)
  return burl

getConfigFromEnvironment :: IO Config
getConfigFromEnvironment = do
  configFilePath <- E.lookupEnv "OPENERGY_OFFER_SERVICE_CONFIG_FILE" >>= pure . fromMaybe "./op-energy-offer-service-config.json"
  configStr <- BS.readFile configFilePath
  case A.eitherDecodeStrict configStr of
    Left some -> error $ configFilePath ++ " is not a valid config: " ++ some
    Right config -> return config
