{--
 - This module defines data type that keep all the metrics handlers
 -}
module OpEnergy.Account.Server.V1.Metrics where

import           Control.Monad.IO.Class(MonadIO)
import           Control.Concurrent.MVar(MVar)
import qualified Control.Concurrent.MVar as MVar

import qualified Prometheus as P
import qualified Network.Wai.Middleware.Prometheus as P
import qualified Prometheus.Metric.GHC as P
import qualified Prometheus.Metric.Proc as P
import qualified Network.Wai.Handler.Warp as W

import           OpEnergy.Account.Server.V1.Config
import           Data.OpEnergy.API.V1.Positive


-- | defines the whole state used by backend
data MetricsState = MetricsState
  { accountLogin :: P.Histogram
  , accountDBLookup :: P.Histogram
  , accountInsert :: P.Histogram
  , accountRegister :: P.Histogram
  , accountPostDisplayName :: P.Histogram
  , accountUpdateLoginsCount :: P.Histogram
  , accountTokenEncrypt :: P.Histogram
  , accountTokenDecrypt :: P.Histogram
  , accountMgetPersonByHashedSecret :: P.Histogram
  , accountMgetPersonByHashedSecretUpdatingTokenCookie :: P.Histogram
  , accountMgetPersonByHashedSecretTokenCookie :: P.Histogram
  , accountMgetPersonByAccountToken :: P.Histogram
  , getBlockTimeStrikeFuture :: P.Histogram
  , createBlockTimeStrikeFuture :: P.Histogram
  , getBlockTimeStrikePast :: P.Histogram
  , getBlockTimeStrikeFutureGuesses :: P.Histogram
  , mgetBlockTimeStrikeFuture :: P.Histogram
  , createBlockTimeStrikeFutureGuess :: P.Histogram
  , mgetBlockTimeStrikePast :: P.Histogram
  , ensureFutureStrikeExistsAhead :: P.Histogram
  }

-- | constructs default state with given config and DB pool
initMetrics :: MonadIO m => Config-> m MetricsState
initMetrics _config = do
  accountLogin <- P.register $ P.histogram (P.Info "accountLogin" "") microBuckets
  accountDBLookup <- P.register $ P.histogram (P.Info "accountDBLookup" "") microBuckets
  accountInsert <- P.register $ P.histogram (P.Info "accountInsert" "") microBuckets
  accountRegister <- P.register $ P.histogram (P.Info "register" "") microBuckets
  accountUpdateLoginsCount <- P.register $ P.histogram (P.Info "accountUpdateLoginsCount" "") microBuckets
  accountPostDisplayName <- P.register $ P.histogram (P.Info "accountPostDisplayName" "") microBuckets
  accountTokenEncrypt <- P.register $ P.histogram (P.Info "accountTokenEncrypt" "") microBuckets
  accountTokenDecrypt <- P.register $ P.histogram (P.Info "accountTokenDecrypt" "") microBuckets
  accountMgetPersonByHashedSecret <- P.register $ P.histogram (P.Info "accountMgetPersonByHashedSecret" "") microBuckets
  accountMgetPersonByHashedSecretUpdatingTokenCookie <- P.register $ P.histogram (P.Info "accountMgetPersonByHashedSecretUpdatingTokenCookie" "") microBuckets
  accountMgetPersonByHashedSecretTokenCookie <- P.register $ P.histogram (P.Info "accountMgetPersonByHashedSecretTokenCookie" "") microBuckets
  accountMgetPersonByAccountToken <- P.register $ P.histogram (P.Info "accountMgetPersonByAccountToken" "") microBuckets
  getBlockTimeStrikeFuture <- P.register $ P.histogram (P.Info "getBlockTimeStrikeFuture" "") microBuckets
  createBlockTimeStrikeFuture <- P.register $ P.histogram (P.Info "createBlockTimeStrikeFuture" "") microBuckets
  getBlockTimeStrikePast <- P.register $ P.histogram (P.Info "getBlockTimeStrikePast" "") microBuckets
  getBlockTimeStrikeFutureGuesses <- P.register $ P.histogram (P.Info "getBlockTimeStrikeFutureGuesses" "") microBuckets
  mgetBlockTimeStrikeFuture <- P.register $ P.histogram (P.Info "mgetBlockTimeStrikeFuture" "") microBuckets
  createBlockTimeStrikeFutureGuess <- P.register $ P.histogram (P.Info "createBlockTimeStrikeFutureGuess" "") microBuckets
  mgetBlockTimeStrikePast <- P.register $ P.histogram (P.Info "mgetBlockTimeStrikePast" "") microBuckets
  ensureFutureStrikeExistsAhead <- P.register $ P.histogram (P.Info "ensureFutureStrikeExistsAhead" "") microBuckets
  _ <- P.register P.ghcMetrics
  _ <- P.register P.procMetrics
  return $ MetricsState
    { accountLogin = accountLogin
    , accountDBLookup = accountDBLookup
    , accountInsert = accountInsert
    , accountRegister = accountRegister
    , accountUpdateLoginsCount = accountUpdateLoginsCount
    , accountPostDisplayName = accountPostDisplayName
    , accountTokenDecrypt = accountTokenDecrypt
    , accountTokenEncrypt = accountTokenEncrypt
    , accountMgetPersonByHashedSecret = accountMgetPersonByHashedSecret
    , accountMgetPersonByHashedSecretUpdatingTokenCookie = accountMgetPersonByHashedSecretUpdatingTokenCookie
    , accountMgetPersonByHashedSecretTokenCookie = accountMgetPersonByHashedSecretTokenCookie
    , accountMgetPersonByAccountToken = accountMgetPersonByAccountToken
    , getBlockTimeStrikeFuture = getBlockTimeStrikeFuture
    , createBlockTimeStrikeFuture = createBlockTimeStrikeFuture
    , getBlockTimeStrikePast = getBlockTimeStrikePast
    , getBlockTimeStrikeFutureGuesses = getBlockTimeStrikeFutureGuesses
    , mgetBlockTimeStrikeFuture = mgetBlockTimeStrikeFuture
    , createBlockTimeStrikeFutureGuess = createBlockTimeStrikeFutureGuess
    , mgetBlockTimeStrikePast = mgetBlockTimeStrikePast
    , ensureFutureStrikeExistsAhead = ensureFutureStrikeExistsAhead
    }
  where
    microBuckets = [ 0.0000001 -- 100 nanoseconds
                   , 0.00000025 -- 250 ns
                   , 0.0000005 -- 500 ns
                   , 0.000001 -- 1 microsecond
                   , 0.00001 -- 10 microseconds
                   , 0.0001 -- 100 microseconds
                   , 0.00025 -- 250 microseconds
                   , 0.0005 -- 500 microseconds
                   , 0.001 -- 1 ms
                   ] ++ P.defaultBuckets

-- | runs metrics HTTP server
runMetricsServer :: Config -> MVar MetricsState -> IO ()
runMetricsServer config metricsV = do
  let Config{configPrometheusPort = metricsPort } = config
  metrics <- initMetrics config
  MVar.putMVar metricsV metrics
  W.run (fromPositive metricsPort) P.metricsApp
