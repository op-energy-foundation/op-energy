{--
 - This module defines data type that keep all the state, used by backend
 -}
{-# LANGUAGE TemplateHaskell #-}
module OpEnergy.Account.Server.V1.Class where

import           Data.Text(Text)
import qualified Data.Text as Text
import           Data.Map(Map)
import           Control.Concurrent.STM.TVar (TVar)
import qualified Control.Concurrent.STM.TVar as TVar
import           Control.Monad.Trans.Reader (runReaderT, ReaderT, ask, asks, local)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans(lift)
import           Control.Monad.Logger (runLoggingT, filterLogger, LoggingT, MonadLoggerIO, Loc, LogSource, LogLevel, LogStr, logError, NoLoggingT)
import           Servant (Handler)
import           Data.Pool(Pool)
import           Database.Persist.Postgresql (SqlBackend, runSqlPersistMPool, runSqlPoolNoTransaction)
import           Control.Monad.Trans.Resource

import           Prometheus(MonadMonitor(..))
import qualified Prometheus as P
import           Control.Exception.Safe (SomeException)
import qualified Control.Exception.Safe as E

import           Data.OpEnergy.Account.API.V1.Account
import qualified OpEnergy.BlockTimeStrike.Server.V1.Class as BlockTime
import           OpEnergy.Account.Server.V1.Config
import           OpEnergy.Account.Server.V1.Metrics

instance MonadMonitor Handler where
  doIO = liftIO

type LogFunc = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

type AccountsCache = Map AccountToken Person
-- | defines the whole state used by backend
data State = State
  { config :: Config
  -- ^ app config, loaded from file
  , accountDBPool :: Pool SqlBackend
  -- ^ DB connection pool to BlockHeadersDB
  , logFunc :: LogFunc
  , logLevel :: TVar LogLevel
  , metrics :: MetricsState
  -- ^ contains metrics handlers
  , blockTimeState :: BlockTime.State
  -- ^ block time strike state
  , callStack :: Text
  -- ^ dot separated labels
  }

type AppT = ReaderT State
type AppM = ReaderT State Handler

-- | constructs default state with given config and DB pool
defaultState :: (MonadLoggerIO m ) => Config-> MetricsState-> LogFunc-> Pool SqlBackend-> m State
defaultState config metrics logFunc accountDBPool = do
  logLevelV <- liftIO $ TVar.newTVarIO (configLogLevelMin config)
  blockTimeState <- BlockTime.defaultState config
  return $ State
    { config = config
    , accountDBPool = accountDBPool
    , logFunc = logFunc
    , logLevel = logLevelV
    , metrics = metrics
    , blockTimeState = blockTimeState
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
  -> (ReaderT SqlBackend (NoLoggingT (ResourceT IO))) r
  -> IO (Maybe r)
withDBTransactionIO state header next = profileM newHeader metricsV $ do
  E.handle (\(err::SomeException) -> do
               runLoggingIO state $ $(logError) (newHeader <> ": " <> Text.pack (show err))
               return Nothing
           ) $ flip runSqlPersistMPool pool (Just <$> next)
  where
    newHeader = header <> ".DBT"
    metricsV = dynamicHistograms (metrics state)
    pool = accountDBPool state

withDBTransaction
  :: ( MonadIO m
     , MonadMonitor m
     )
  => Text
  -> (ReaderT SqlBackend (NoLoggingT (ResourceT IO))) r
  -> AppT m (Maybe r)
withDBTransaction name next = profile name $ do
  header <- asks callStack
  state <- ask
  liftIO $ withDBTransactionIO state header next

withDBNOTransactionROIO
  :: State
  -> Text
  -> (ReaderT SqlBackend IO) r
  -> IO (Maybe r)
withDBNOTransactionROIO state header next = profileM newHeader metricsV $ do
  E.handle (\(err::SomeException) -> do
               runLoggingIO state $ $(logError) (newHeader <> ": " <> Text.pack (show err))
               return Nothing
           ) $ runSqlPoolNoTransaction (Just <$> next) pool Nothing
  where
    newHeader = header <> ".DBT"
    metricsV = dynamicHistograms (metrics state)
    pool = accountDBPool state

withDBNOTransactionRO
  :: ( MonadIO m
     , MonadMonitor m
     )
  => Text
  -> (ReaderT SqlBackend IO) r
  -> AppT m (Maybe r)
withDBNOTransactionRO name next = profile name $ do
  header <- asks callStack
  state <- ask
  liftIO $ withDBNOTransactionROIO state header next
