{--
 - This module defines data type that keep all the metrics handlers
 -}
module OpEnergy.Offer.Server.V1.Metrics where

import           Data.Text(Text)
import           Control.Monad.IO.Class(MonadIO)
import           Control.Concurrent.MVar(MVar)
import qualified Control.Concurrent.MVar as MVar
import           Control.Concurrent.STM.TVar(TVar)
import qualified Control.Concurrent.STM as STM
import           Data.Map (Map)
import qualified Data.Map as Map
import           Control.Monad.IO.Class(liftIO)

import qualified Prometheus as P
import qualified Network.Wai.Middleware.Prometheus as P
import qualified Prometheus.Metric.GHC as P
import qualified Prometheus.Metric.Proc as P
import qualified Network.Wai.Handler.Warp as W

import           OpEnergy.Offer.Server.V1.Config
import           Data.OpEnergy.API.V1.Positive


-- | defines the whole state used by backend
data MetricsState = MetricsState
  { offerPost :: P.Histogram
  , offerGetMine :: P.Histogram
  , offerGetList :: P.Histogram
  , offerGetById :: P.Histogram
  , offerCancel :: P.Histogram
  , offerExpireSweep :: P.Histogram
  , accountVerifyToken :: P.Histogram
    -- ^ latency of the cross-service call to oe-account-service's /whoami
  , dynamicHistograms :: TVar (Map Text P.Histogram)
  }

-- | constructs default state with given config and DB pool
initMetrics :: MonadIO m => Config-> m MetricsState
initMetrics _config = do
  offerPost <- P.register $ P.histogram (P.Info "offerPost" "") microBuckets
  offerGetMine <- P.register $ P.histogram (P.Info "offerGetMine" "") microBuckets
  offerGetList <- P.register $ P.histogram (P.Info "offerGetList" "") microBuckets
  offerGetById <- P.register $ P.histogram (P.Info "offerGetById" "") microBuckets
  offerCancel <- P.register $ P.histogram (P.Info "offerCancel" "") microBuckets
  offerExpireSweep <- P.register $ P.histogram (P.Info "offerExpireSweep" "") microBuckets
  accountVerifyToken <- P.register $ P.histogram (P.Info "accountVerifyToken" "") microBuckets
  _ <- P.register P.ghcMetrics
  _ <- P.register P.procMetrics
  tmap <- liftIO $ STM.newTVarIO (Map.empty)
  return $ MetricsState
    { offerPost = offerPost
    , offerGetMine = offerGetMine
    , offerGetList = offerGetList
    , offerGetById = offerGetById
    , offerCancel = offerCancel
    , offerExpireSweep = offerExpireSweep
    , accountVerifyToken = accountVerifyToken
    , dynamicHistograms = tmap
    }

microBuckets :: [Double]
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

dynamicHistogram :: TVar (Map Text P.Histogram) -> Text -> IO P.Histogram
dynamicHistogram tmap name = do
  map <- STM.atomically $ STM.readTVar tmap
  case Map.lookup name map of
    Just some -> return some
    Nothing-> do
      ret <- P.register $ P.histogram (P.Info name "") microBuckets
      STM.atomically $ STM.modifyTVar tmap $ Map.insert name ret
      return ret
