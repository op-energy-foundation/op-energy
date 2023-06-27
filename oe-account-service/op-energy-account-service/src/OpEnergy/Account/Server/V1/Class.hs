{--
 - This module defines data type that keep all the state, used by backend
 -}
module OpEnergy.Account.Server.V1.Class where

import           Data.Map(Map)
import           Control.Concurrent.STM.TVar (TVar)
import qualified Control.Concurrent.STM.TVar as TVar
import           Control.Monad.Trans.Reader (runReaderT, ReaderT, ask)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans(lift)
import           Control.Monad.Logger (runLoggingT, filterLogger, LoggingT, MonadLoggerIO, Loc, LogSource, LogLevel, LogStr)
import           Servant (Handler)
import           Data.Pool(Pool)
import           Database.Persist.Postgresql (SqlBackend)

import           Prometheus(MonadMonitor(..))

import           Data.OpEnergy.Account.API.V1.Account
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
  }

type AppT = ReaderT State
type AppM = ReaderT State Handler

-- | constructs default state with given config and DB pool
defaultState :: (MonadLoggerIO m ) => Config-> MetricsState-> LogFunc-> Pool SqlBackend-> m State
defaultState config metrics logFunc accountDBPool = do
  logLevelV <- liftIO $ TVar.newTVarIO (configLogLevelMin config)
  return $ State
    { config = config
    , accountDBPool = accountDBPool
    , logFunc = logFunc
    , logLevel = logLevelV
    , metrics = metrics
    }

-- | Runs app transformer with given context
runAppT :: (Monad m) => State-> AppT m a-> m a
runAppT s x = runReaderT x s

runLogging :: MonadIO m => LoggingT m a -> AppT m ()
runLogging loggingAction = do
  State{ logFunc = logFunc, config = Config{ configLogLevelMin = logLevelMin}} <- ask
  let filterUnwantedLevels _source level = level >= logLevelMin
  _ <- lift $ runLoggingT (filterLogger filterUnwantedLevels loggingAction) logFunc
  return ()
