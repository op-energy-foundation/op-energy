{--
 - This module defines data type that keep all the state, used by backend
 -}
{-# LANGUAGE TemplateHaskell #-}
module OpEnergy.Offer.Server.V1.Class where

import           Data.Text(Text)
import qualified Data.Text as Text
import           Data.Map(Map)
import qualified Control.Concurrent.STM.TVar as TVar
import           Control.Concurrent.STM.TVar (TVar)
import           Control.Monad.Trans.Reader (runReaderT, ReaderT, ask, asks, local)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans(lift)
import           Control.Monad.Logger (runLoggingT, filterLogger, LoggingT, MonadLoggerIO, Loc, LogSource, LogLevel, LogStr, logError, NoLoggingT)
import           Servant (Handler)
import           Data.Pool(Pool)
import           Database.Persist.Postgresql (SqlBackend, runSqlPersistMPool)
import           Control.Monad.Trans.Resource

import           Prometheus(MonadMonitor(..))
import qualified Prometheus as P
import           Control.Exception.Safe (SomeException)
import qualified Control.Exception.Safe as E

import           Data.OpEnergy.API.V1.Block(BlockHeight)
import           OpEnergy.Offer.Server.V1.Config
import           OpEnergy.Offer.Server.V1.Metrics

instance MonadMonitor Handler where
  doIO = liftIO

type LogFunc = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

-- | defines the whole state used by backend
data State = State
  { config :: Config
  , offerDBPool :: Pool SqlBackend
  , logFunc :: LogFunc
  , logLevel :: TVar LogLevel
  , metrics :: MetricsState
  , currentTip :: TVar (Maybe BlockHeight)
    -- ^ best-effort current chain tip. Never populated by this port --
    -- wiring a live tip source is future work.
  , callStack :: Text
  }

type AppT = ReaderT State
type AppM = ReaderT State Handler

-- | constructs default state with given config and DB pool
defaultState :: (MonadLoggerIO m ) => Config-> MetricsState-> LogFunc-> Pool SqlBackend-> m State
defaultState config metrics logFunc offerDBPool = do
  logLevelV <- liftIO $ TVar.newTVarIO (configLogLevelMin config)
  currentTipV <- liftIO $ TVar.newTVarIO Nothing
  return $ State
    { config = config
    , offerDBPool = offerDBPool
    , logFunc = logFunc
    , logLevel = logLevelV
    , metrics = metrics
    , currentTip = currentTipV
    , callStack = ""
    }

-- | Runs app transformer with given context
runAppT :: (Monad m) => State-> AppT m a-> m a
runAppT s x = runReaderT x s

runLoggingIO :: State -> LoggingT IO a -> IO ()
runLoggingIO state loggingAction = do
  let
      State{ logFunc = logFunc, config = Config{ configLogLevelMin = logLevelMin}} = state
  let filterUnwantedLevels _source level = level >= logLevelMin
  _ <- runLoggingT (filterLogger filterUnwantedLevels loggingAction) logFunc
  return ()

runLogging :: MonadIO m => LoggingT m a -> AppT m ()
runLogging loggingAction = do
  State{ logFunc = logFunc, config = Config{ configLogLevelMin = logLevelMin}} <- ask
  let filterUnwantedLevels _source level = level >= logLevelMin
  _ <- lift $ runLoggingT (filterLogger filterUnwantedLevels loggingAction) logFunc
  return ()

profile
  :: ( MonadIO m
     , MonadMonitor m
     )
  => Text
  -> AppT m r
  -> AppT m r
profile name next = do
  metricsV <- asks metrics
  callstackV <- asks callStack
  let
      newCallStack = callstackV <> "." <> name
  local (\r-> r{ callStack = newCallStack}) $ do
    profileM newCallStack (dynamicHistograms metricsV) next

profileM
  :: ( MonadIO m
     , MonadMonitor m
     )
  => Text
  -> TVar (Map Text P.Histogram)
  -> m r
  -> m r
profileM callstackV metricsV next = do
  histogram <- liftIO $ dynamicHistogram metricsV callstackV
  P.observeDuration histogram next

withDBTransactionIO
  :: State
  -> Text
  -> (ReaderT SqlBackend (Control.Monad.Logger.NoLoggingT (ResourceT IO))) r
  -> IO (Maybe r)
withDBTransactionIO state header next = profileM newHeader metricsV $ do
  E.handle (\(err::SomeException) -> do
               runLoggingIO state $ $(logError) (newHeader <> ": " <> Text.pack (show err))
               return Nothing
           ) $ flip runSqlPersistMPool pool (Just <$> next)
  where
    newHeader = header <> ".DBT"
    metricsV = dynamicHistograms (metrics state)
    pool = offerDBPool state

withDBTransaction
  :: ( MonadIO m
     , MonadMonitor m
     )
  => Text
  -> (ReaderT SqlBackend (Control.Monad.Logger.NoLoggingT (ResourceT IO))) r
  -> AppT m (Maybe r)
withDBTransaction name next = profile name $ do
  header <- asks callStack
  state <- ask
  liftIO $ withDBTransactionIO state header next
