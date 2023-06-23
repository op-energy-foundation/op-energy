{--
 - This module defines data type that keep all the metrics handlers
 -}
module OpEnergy.Account.Server.V1.Metrics where

-- import           System.Clock (Clock(..), diffTimeSpec, getTime, toNanoSecs)
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
  { loadDBState :: P.Histogram
  , accountLogin :: P.Histogram
  , accountDBLookup :: P.Histogram
  , accountInsert :: P.Histogram
  , register :: P.Histogram
  }

-- | constructs default state with given config and DB pool
initMetrics :: MonadIO m => Config-> m MetricsState
initMetrics _config = do
  loadDBState <- P.register $ P.histogram (P.Info "loadDBState" "") microBuckets
  accountLogin <- P.register $ P.histogram (P.Info "accountLogin" "") microBuckets
  accountDBLookup <- P.register $ P.histogram (P.Info "accountDBLookup" "") microBuckets
  accountInsert <- P.register $ P.histogram (P.Info "accountInsert" "") microBuckets
  register <- P.register $ P.histogram (P.Info "register" "") microBuckets
  _ <- P.register P.ghcMetrics
  _ <- P.register P.procMetrics
  return $ MetricsState
    { loadDBState = loadDBState
    , accountLogin = accountLogin
    , accountDBLookup = accountDBLookup
    , accountInsert = accountInsert
    , register = register
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
