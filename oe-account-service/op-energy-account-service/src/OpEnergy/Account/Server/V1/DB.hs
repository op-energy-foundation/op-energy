{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls  #-}


module OpEnergy.Account.Server.V1.DB where

import           Data.Pool
import qualified Data.Text.Show as T
import qualified Data.Text.Encoding as TE
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.IO.Unlift(MonadUnliftIO)
import           Control.Monad.Logger    (MonadLoggerIO )

import           Database.Persist.Postgresql

import           OpEnergy.Account.Server.V1.Config
import           Data.OpEnergy.Account.API.V1.Account
import           Data.OpEnergy.API.V1.Positive(fromPositive)

-- | connect to DB. Returns connection pool
getConnection
  :: ( MonadLoggerIO m
     , MonadUnliftIO m
     , MonadIO m
     )
  => Config
  -> m (Pool SqlBackend)
getConnection config = do
  pool <- createPostgresqlPool
    connStr
    (fromPositive $ configDBConnectionPoolSize config)
  liftIO $ flip runSqlPersistMPool pool $ runMigration migrateAccount -- perform necessary migrations. currently only BlockHeader table's migrations
  return pool
  where
    connStr = TE.encodeUtf8
      $! "host=" <> configDBHost config
      <> " port=" <> (T.tshow $ configDBPort config)
      <> " user=" <> configDBUser config
      <> " dbname=" <> configDBName config
      <> " password=" <> configDBPassword config
