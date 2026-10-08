{-# LANGUAGE OverloadedStrings #-}
module OpEnergy.Account.Server.V1.Config where

import           Data.Int (Int64)
import qualified Data.List as List
import           Data.Text (Text)
import           Data.Word (Word32, Word64)
import qualified Data.Text.Encoding as Text
import           Data.Maybe
import qualified Data.ByteString.Char8 as BS
import qualified System.Environment as E
import           Data.OpEnergy.Account.API.V1.Sats (Sats(..))
import           Data.OpEnergy.Account.API.V2.WalletMode
                 ( WalletMode(..)
                 )
import           Data.OpEnergy.API.V1.Positive
import           Control.Monad.Catch
import           Control.Monad.Logger(LogLevel(..))

import           Data.Aeson(FromJSON, withText, withObject, (.:?), (.!=))
import           Data.Aeson.Types (Parser)
import qualified Data.Aeson as A
import           Web.ClientSession (Key)
import qualified Web.ClientSession as ClientSession
import qualified Data.ByteString.Base64 as Base64
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
  , configAccountTokenEncryptionPrivateKey :: Key
    -- ^ secret key used to encrypt/decrypt AccountToken
  , configBlockTimeStrikeMinimumBlockAheadCurrentTip :: Positive Int
    -- ^ this value defines the minimum amount of blocks that block time strike should be ahead of current tip to be accepted to be created
  , configBlockTimeStrikeBlockSpanWebsocketAPIURL :: BaseUrl
    -- ^ defines URL of blockspan API service
  , configBlockTimeStrikeGuessMinimumBlockAheadCurrentTip :: Positive Int
    -- ^ this value defines the minimum ahead of  strike guess to require. It should be at least 6, as confirmed current tip is 6 blocks behind unconfirmed tip
  , configBlockspanURL :: BaseUrl
    -- ^ defines URL to blockspan api instance. Used to get block header info at block discovery
  , configBlockTimeStrikeShouldExistsAheadCurrentTip :: Positive Int
    -- ^ defines how many blocks ahead of current tip there should exist a  strike. Effectively, this means, that there should exist  strikes for a range [ currentTip + configBlockTimeStrikeGuessMinimumBlockAheadCurrentTip; currentTip + configBlockTimeStrikeGuessMinimumBlockAheadCurrentTip + configBlockTimeStrikeShouldExistsAheadCurrentTip ]
  , configRecordsPerReply :: Positive Int
    -- ^ defines how much records should be returned in one page
  , configAverageBlockDiscoverSecs :: Positive Int
    -- ^ defines how much seconds takes to discover in average
  , configBlockSpanDefaultSize :: Positive Int
    -- ^ default block span size to use when user does not provide one
  , configInternalServiceSharedSecret :: Text
    -- ^ shared secret checked against the X-Internal-Service-Secret header
    -- on internal balance endpoints. Must match the value configured into
    -- any caller (e.g. oe-offer-service).
  , configStartingBalanceSats :: Word64
    -- ^ sandbox wallet balance assigned to newly registered accounts
  , configWalletBackend :: WalletMode
    -- ^ which wallet the service talks to: "mock" keeps invoices and
    -- payments in this service's own database, "lnbits" talks to an LNBits
    -- instance in front of a lightning node
  , configWalletMinInvoiceSats :: Sats
    -- ^ smallest amount an invoice may be created for
  , configWalletMaxInvoiceSats :: Sats
    -- ^ largest amount an invoice may be created for
  , configWalletMaxWithdrawalSats :: Sats
    -- ^ largest amount one payment may send out of an account
  , configWalletInvoiceExpirySecs :: Word64
    -- ^ how long an invoice is advertised as payable. It is recorded and
    -- reported to the client; refusing an expired invoice is the lightning
    -- node's job, so the mock wallet, which has none, does not
  , configWalletRecordsPerPage :: Word32
    -- ^ how many movements one page of the wallet history holds
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
    <*> ( v .:? "ACCOUNT_TOKEN_ENCRYPTION_PRIVATE_KEY" .!= (configAccountTokenEncryptionPrivateKey defaultConfig))
    <*> ( v .:? "BLOCKTIME_STRIKE_MINIMUM_BLOCKS_AHEAD_CURRENT_TIP" .!= (configBlockTimeStrikeMinimumBlockAheadCurrentTip defaultConfig))
    <*> ((v .:? "BLOCKTIME_STRIKE_BLOCKSPAN_WEBSOCKET_API_URL" .!= (showBaseUrl $ configBlockTimeStrikeBlockSpanWebsocketAPIURL defaultConfig)) >>= parseBaseUrl)
    <*> ( v .:? "BLOCKTIME_STRIKE_GUESS_MINIMUM_BLOCKS_AHEAD_CURRENT_TIP" .!= (configBlockTimeStrikeGuessMinimumBlockAheadCurrentTip defaultConfig))
    <*> ((v .:? "BLOCKSPAN_API_URL" .!= (showBaseUrl $ configBlockspanURL defaultConfig)) >>= parseBaseUrl)
    <*> ( v .:? "BLOCKTIME_STRIKE_SHOULD_EXISTS_AHEAD_CURRENT_TIP" .!= (configBlockTimeStrikeShouldExistsAheadCurrentTip defaultConfig))
    <*> ( v .:? "BLOCKTIME_RECORDS_PER_REPLY" .!= (configRecordsPerReply defaultConfig))
    <*> ( v .:? "AVERAGE_BLOCK_DISCOVER_SECS" .!= (configAverageBlockDiscoverSecs defaultConfig))
    <*> ( v .:? "BLOCKSPAN_DEFAULT_SIZE" .!= (configBlockSpanDefaultSize defaultConfig))
    <*> ( v .:? "INTERNAL_SERVICE_SHARED_SECRET" .!= (configInternalServiceSharedSecret defaultConfig))
    <*> ( v .:? "STARTING_BALANCE_SATS" .!= (configStartingBalanceSats defaultConfig))
    <*> ( v .:? "WALLET_BACKEND" .!= (configWalletBackend defaultConfig))
    <*> ( v .:? "WALLET_MIN_INVOICE_SATS" .!= (configWalletMinInvoiceSats defaultConfig))
    <*> ( v .:? "WALLET_MAX_INVOICE_SATS" .!= (configWalletMaxInvoiceSats defaultConfig))
    <*> ( v .:? "WALLET_MAX_WITHDRAWAL_SATS" .!= (configWalletMaxWithdrawalSats defaultConfig))
    <*> ( v .:? "WALLET_INVOICE_EXPIRY_SECS" .!= (configWalletInvoiceExpirySecs defaultConfig))
    <*> ( v .:? "WALLET_RECORDS_PER_PAGE" .!= (configWalletRecordsPerPage defaultConfig))

-- need to get Key from json, which represented as base64-encoded string
instance FromJSON Key where
  parseJSON = withText "Key" $ \v-> case Base64.decode $! Text.encodeUtf8 v of
    Right some -> case ClientSession.initKey some of
      Right key -> return key
      Left err -> error ("ERROR: FromJSON Key/ClientSession.initKey: " ++ err ++ ". Please generate it with \"dd if=/dev/urandom bs=1 count=96 2>/dev/null | base64 -w 0\" command")
    Left err -> error ("ERROR: FromJSON Key: " ++ err)

defaultConfig:: Config
defaultConfig = Config
  { configDBPort = 5432
  , configDBHost = "localhost"
  , configDBUser = "openergy"
  , configDBName = "openergyacc"
  , configDBPassword = ""
  , configDBConnectionPoolSize = 32
  , configSalt = ""
  , configHTTPAPIPort = 8899
  , configSchedulerPollRateSecs = verifyPositive 1
  , configWebsocketKeepAliveSecs = 10
  , configLogLevelMin = LevelWarn
  , configPrometheusPort = 7899
  , configAccountTokenEncryptionPrivateKey = error "defaultConfig: you are missing ACCOUNT_TOKEN_ENCRYPTION_PRIVATE_KEY from config. Please generate it with \"dd if=/dev/urandom bs=1 count=96 2>/dev/null | base64 -w 0\" command"
  , configBlockTimeStrikeMinimumBlockAheadCurrentTip = 12 -- 6 blocks gives us the unconfirmed tip and 6 more gives minimum barrier ahead
  , configBlockTimeStrikeBlockSpanWebsocketAPIURL = BaseUrl Http "127.0.0.1" 8999 "/api/v1/ws"
  , configBlockTimeStrikeGuessMinimumBlockAheadCurrentTip = 6
  , configBlockspanURL = BaseUrl Http "127.0.0.1" 8999 ""
  , configBlockTimeStrikeShouldExistsAheadCurrentTip = 12
  , configRecordsPerReply = 100
  , configAverageBlockDiscoverSecs = 600
  , configBlockSpanDefaultSize = verifyPositive 24
  , configInternalServiceSharedSecret = error "defaultConfig: you are missing INTERNAL_SERVICE_SHARED_SECRET from config. Generate with \"dd if=/dev/urandom bs=1 count=32 2>/dev/null | base64 -w 0\" command"
  , configStartingBalanceSats = 300000
  , configWalletBackend = WalletModeMock
  , configWalletMinInvoiceSats = Sats 1
  , configWalletMaxInvoiceSats = Sats 10000000
  , configWalletMaxWithdrawalSats = Sats 10000000
  , configWalletInvoiceExpirySecs = 3600
  , configWalletRecordsPerPage = 10
  }

-- | the largest amount, which survives a round trip through the DB:
-- PersistField Sats stores a Word64 as an Int64, so a configured bound
-- above this is stored as a negative amount and compares the wrong way
maxStorableSats :: Sats
maxStorableSats = Sats (fromIntegral (maxBound :: Int64))

-- | the most rows one page of the wallet history may hold. A page is read
-- with an OFFSET of the requested page times this, and the page a client
-- asks for is a Word32, so this also keeps that product inside an Int
maxWalletRecordsPerPage :: Word32
maxWalletRecordsPerPage = 1000

-- | checks the wallet options against each other, so a config, which can
-- not work, stops the service at startup instead of at the first request it
-- breaks. Every problem is reported at once, rather than one per restart.
--
-- Example:
--
-- > everifyWalletConfig defaultConfig == Right ()
everifyWalletConfig :: Config -> Either String ()
everifyWalletConfig config
  | List.null problems = Right ()
  | otherwise = Left (List.intercalate "; " problems)
  where
    problems = catMaybes
      [ require (configWalletRecordsPerPage config > 0)
          "WALLET_RECORDS_PER_PAGE must be above 0: a page of no rows is \
          \queried without a limit, which answers with the whole history"
      , require (configWalletRecordsPerPage config <= maxWalletRecordsPerPage)
          ( "WALLET_RECORDS_PER_PAGE must not be above "
         <> show maxWalletRecordsPerPage
         <> ": a page of more rows than that answers with as much of the \
            \history as a single reply can carry, which is what bounding \
            \it at all is for"
          )
      , require (configWalletMinInvoiceSats config > Sats 0)
          "WALLET_MIN_INVOICE_SATS must be above 0: an amount of nothing \
          \moves no balance, while it still records a payment and a ledger \
          \entry"
      , require
          (configWalletMinInvoiceSats config
            <= configWalletMaxInvoiceSats config)
          "WALLET_MIN_INVOICE_SATS must not be above WALLET_MAX_INVOICE_SATS: \
          \no amount would be accepted, while the wallet still reports the \
          \range as the one it takes"
      , require (configWalletMaxInvoiceSats config <= maxStorableSats)
          "WALLET_MAX_INVOICE_SATS must not be above the largest amount the \
          \DB stores"
      , require (configWalletMaxWithdrawalSats config > Sats 0)
          "WALLET_MAX_WITHDRAWAL_SATS must be above 0: no payment would be \
          \accepted"
      , require (configWalletMaxWithdrawalSats config <= maxStorableSats)
          "WALLET_MAX_WITHDRAWAL_SATS must not be above the largest amount \
          \the DB stores"
      , require (configWalletInvoiceExpirySecs config > 0)
          "WALLET_INVOICE_EXPIRY_SECS must be above 0: an invoice would be \
          \advertised as expired as soon as it is created"
      ]
    require holds message = if holds then Nothing else Just message

getConfigFromEnvironment :: IO Config
getConfigFromEnvironment = do
  configFilePath <- E.lookupEnv "OPENERGY_ACCOUNT_SERVICE_CONFIG_FILE" >>= pure . fromMaybe "./op-energy-account-service-config.json"
  configStr <- BS.readFile configFilePath
  case A.eitherDecodeStrict configStr of
    Left some -> error $ configFilePath ++ " is not a valid config: " ++ some
    Right config -> case everifyWalletConfig config of
      Left problem ->
        error $ configFilePath ++ " is not a usable config: " ++ problem
      Right () -> return config
