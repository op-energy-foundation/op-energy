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
module OpEnergy.Offer.Server.V1.DB where

import           Data.Pool
import qualified Data.Text.Show as T
import qualified Data.Text.Encoding as TE
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.IO.Unlift(MonadUnliftIO)
import           Control.Monad.Logger    (MonadLoggerIO, NoLoggingT )
import           Control.Monad.Trans.Reader (ReaderT)
import           Control.Monad ( forM_)
import           Control.Monad.Trans.Resource(ResourceT)
import qualified Data.List as List

import           Database.Persist.Postgresql

import           Data.OpEnergy.API.V1.Positive(fromPositive)
import           Data.OpEnergy.API.V1.Natural(verifyNatural, fromNatural)

import           OpEnergy.Offer.Server.V1.Config
import           OpEnergy.Offer.Server.V1.DB.Migrations
import           OpEnergy.Offer.Server.V1.Offer

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
  liftIO $ flip runSqlPersistMPool pool $ do
    printMigration migrateOffer
    migrateOfferDBSchema config
    printMigration migrateOffer
    runMigration migrateOffer

  return pool
  where
    connStr = TE.encodeUtf8
      $! "host=" <> configDBHost config
      <> " port=" <> (T.tshow $ configDBPort config)
      <> " user=" <> configDBUser config
      <> " dbname=" <> configDBName config
      <> " password=" <> configDBPassword config

offerDBMigrations :: [( Config -> ReaderT
                                  SqlBackend
                                  (Control.Monad.Logger.NoLoggingT
                                   (ResourceT IO)
                                  )
                                  ()
                                )
                               ]
offerDBMigrations =
  [ (\_-> return ()) -- dummy, for compatibility reasons
  ]

migrateOfferDBSchema
  :: Config
  -> ReaderT
       SqlBackend
       (NoLoggingT
        (ResourceT IO)
       )
       ()
migrateOfferDBSchema config = do
  runMigration migrateOfferDB
  transactionSave

  let
      dbVersionAfterMigrations = List.length offerDBMigrations
  (currentDBVersion, currentDBVersionId) <- do
    mrecord <- selectFirst [] []
    case mrecord of
      Just (Entity currentDBVersionId record)-> return (offerDBVersion record, currentDBVersionId)
      Nothing -> do
        (mOfferTableExistButVersionUnknown::[Single Bool]) <- rawSql
          "SELECT EXISTS (SELECT FROM information_schema.tables where table_name = ? and table_schema='public');"
          [ PersistText $! unEntityNameDB (tableDBName (undefined :: Offer))]
        let
            currentDBVersion = case mOfferTableExistButVersionUnknown of
              ((Single True):_)  -> needToApplyCustomMigrations
                where
                  needToApplyCustomMigrations = 0
              ((Single False):_)  -> latestSchemaWillBeCreatedByORM
                where
                latestSchemaWillBeCreatedByORM = verifyNatural dbVersionAfterMigrations
              _ -> error ("migrateOfferDBSchema: got unexpected response from DB: " ++ show mOfferTableExistButVersionUnknown)
        currentDBVersionId <- insert $ OfferDB
          { offerDBVersion = currentDBVersion
          }
        return (currentDBVersion, currentDBVersionId)
  let
      unAppliedMigrations = List.drop (fromNatural currentDBVersion) offerDBMigrations
  if dbVersionAfterMigrations < fromNatural currentDBVersion
    then do
      let msg = "migrateOfferDBSchema: unsupported DB schema " ++ show currentDBVersion ++ ", supported DB version up to: " ++ show dbVersionAfterMigrations
      error msg
    else do
      forM_ unAppliedMigrations $ \migration -> do
        migration config
        update currentDBVersionId
          [ OfferDBVersion +=. verifyNatural 1
          ]
        transactionSave
