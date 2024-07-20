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
import           Control.Monad.Logger    (MonadLoggerIO, NoLoggingT )
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Reader (ReaderT)
import           Control.Monad (void, forM_)
import           Control.Monad.Trans.Resource(ResourceT)
import qualified Data.List as List

import           Database.Persist.Postgresql
import qualified Data.Conduit as C
import           Data.Conduit ((.|), runConduit )
import qualified Data.Conduit.List as C
import           Database.Persist.Pagination

import           OpEnergy.Account.Server.V1.Config
import           Data.OpEnergy.Account.API.V1.Account
import           Data.OpEnergy.Account.API.V1.BlockTimeStrike
import           Data.OpEnergy.Account.API.V1.BlockTimeStrikeGuess
import           Data.OpEnergy.API.V1.Positive(fromPositive)
import           Data.OpEnergy.API.V1.Natural(verifyNatural, fromNatural)
import           OpEnergy.Account.Server.V1.DB.Migrations



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
    -- print migrations to stdout just for information to get of schema difference
    printMigration migrateAccount
    printMigration migrateBlockTimeStrike
    printMigration migrateBlockTimeStrikeGuess
    printMigration migrateBlockTimeStrikeDB

    -- at this point we start to run our custom migrations, that can't be handled by persisten itself
    migrateAccountDBSchema config
    migrateBlockTimeStrikeDBSchema config

    -- actually run migrations by persistent
    runMigration migrateAccount -- perform necessary migrations. currently only BlockHeader table's migrations
    runMigration migrateBlockTimeStrike
    runMigration migrateBlockTimeStrikeGuess
    runMigration migrateBlockTimeStrikeDB

  return pool
  where
    connStr = TE.encodeUtf8
      $! "host=" <> configDBHost config
      <> " port=" <> (T.tshow $ configDBPort config)
      <> " user=" <> configDBUser config
      <> " dbname=" <> configDBName config
      <> " password=" <> configDBPassword config

-- | this list contains our custom migrations that can't be generated by persistent, like initial calculating of aggregated fields' values
-- NOTE: this list have to be append only. It is your responsibility to not to delete or change the list after merge. ONLY APPENDS ARE ALLOWED
-- after running each migration DB version is being increased automatically and transaction being commited. Each migration is running in a separate transcation.
-- NOTE: it is expected, that migrations will be idempotent
accountDBMigrations :: [( Config -> ReaderT
                                  SqlBackend
                                  (Control.Monad.Logger.NoLoggingT
                                   (ResourceT IO)
                                  )
                                  ()
                                )
                               ]
accountDBMigrations =
  [ (\_-> runMigration migrateAccount) -- perform initial migration of schema
  ]

-- | custom migration procedure
-- for now, it is expected, that each migration have no clue about db version and thus none
-- of individual migration can affect on the order of the migrations, so there is no way
-- to just to increase version not by 1
migrateAccountDBSchema
  :: Config
  -> ReaderT
       SqlBackend
       (NoLoggingT
        (ResourceT IO)
       )
       ()
migrateAccountDBSchema config = do
  runMigration migrateAccountDB -- ensure, that there should exist version-tacing table
  transactionSave -- commit everything, that happend before we start

  (currentDBVersion, currentDBVersionId) <- do -- try to get latest applied
                                               -- migrations' version
    mrecord <- selectFirst [] []
    case mrecord of
      Just (Entity currentDBVersionId record)-> return (accountDBVersion record, currentDBVersionId)
      Nothing -> do -- fallback to default version of 0
        let
            currentDBVersion = verifyNatural 0
        currentDBVersionId <- insert $ AccountDB
          { accountDBVersion = currentDBVersion
          }
        return (currentDBVersion, currentDBVersionId)
  let
      unAppliedMigrations = List.drop (fromNatural currentDBVersion) accountDBMigrations
  forM_ unAppliedMigrations $ \migration -> do
    migration config -- call the actual migration procedure
    update currentDBVersionId
      [ AccountDBVersion +=. 1
      ]
    transactionSave -- keep each migration as one transaction to keep db at the latest successful migration

-- | this list contains our custom migrations that can't be generated by persistent, like initial calculating of aggregated fields' values
-- NOTE: this list have to be append only. It is your responsibility to not to delete or change the list after merge. ONLY APPENDS ARE ALLOWED
-- after running each migration DB version is being increased automatically and transaction being commited. Each migration is running in a separate transcation.
-- NOTE: it is expected, that migrations will be idempotent
blockTimeStrikeDBMigrations :: [( Config -> ReaderT
                                  SqlBackend
                                  (Control.Monad.Logger.NoLoggingT
                                   (ResourceT IO)
                                  )
                                  ()
                                )
                               ]
blockTimeStrikeDBMigrations =
  -- the very start of the list is a persistent schema migration as it will be applied on a
  -- clean db
  [ (\_-> runMigration migrateBlockTimeStrike)
  , (\_-> runMigration migrateBlockTimeStrikeGuess)
  , (\_-> runMigration migrateBlockTimeStrikeDB) -- create BlockTimeStrikeDB first
    -- here persistent migrations are done and we have an initial schema had been deployed
    -- the rest is expected to be a migrations, that should handle incompatible migrations over some DB schema versions that are already running

    -- initial calculations of the block strike guesses count
  , calculateBlockTimeStrikeCalculateGuessesCount
  ]
  where
    -- | this migration perform calculation of guesses count for existing strike in order to
    -- fill appropriate CalculatedBlockTimeStrikeGuessesCount table
    calculateBlockTimeStrikeCalculateGuessesCount config = do
      runConduit
        $ streamEntities -- search for strikes
          []
          BlockTimeStrikeId
          (PageSize ((fromPositive recordsPerReply) + 1))
          Descend
          (Range Nothing Nothing)
        .| ( C.awaitForever $ \(Entity strikeId _)-> do
             guessesCount <- lift $ verifyNatural <$> count [ BlockTimeStrikeGuessStrike ==. strikeId ] -- count guesses
             mguessesRecord <- lift $ selectFirst [ CalculatedBlockTimeStrikeGuessesCountStrike ==. strikeId ][]
             case mguessesRecord of
               Nothing -> do -- insert new row for appropriate strike
                 void $ lift $ insert $ CalculatedBlockTimeStrikeGuessesCount
                   { calculatedBlockTimeStrikeGuessesCountGuessesCount = guessesCount
                   , calculatedBlockTimeStrikeGuessesCountStrike = strikeId
                   }
               Just (Entity guessesRecordId _)-> do -- update existing strike
                 lift $ update guessesRecordId
                   [ CalculatedBlockTimeStrikeGuessesCountGuessesCount =. guessesCount
                   ]
           )
        .| C.sinkNull
      where
        recordsPerReply = configRecordsPerReply config

-- | custom migration procedure
-- for now, it is expected, that each migration have no clue about db version and thus none
-- of individual migration can affect on the order of the migrations, so there is no way
-- to just to increase version not by 1
migrateBlockTimeStrikeDBSchema
  :: Config
  -> ReaderT
       SqlBackend
       (NoLoggingT
        (ResourceT IO)
       )
       ()
migrateBlockTimeStrikeDBSchema config = do
  runMigration migrateBlockTimeStrikeDB -- ensure, that there should exist version-tacing table
  transactionSave -- commit everything, that happend before we start

  (currentDBVersion, currentDBVersionId) <- do -- try to get latest applied
                                               -- migrations' version
    mrecord <- selectFirst [] []
    case mrecord of
      Just (Entity currentDBVersionId record)-> return (blockTimeStrikeDBVersion record, currentDBVersionId)
      Nothing -> do -- fallback to default version of 0
        let
            currentDBVersion = verifyNatural 0
        currentDBVersionId <- insert $ BlockTimeStrikeDB
          { blockTimeStrikeDBVersion = currentDBVersion
          }
        return (currentDBVersion, currentDBVersionId)
  let
      unAppliedMigrations = List.drop (fromNatural currentDBVersion) blockTimeStrikeDBMigrations
  forM_ unAppliedMigrations $ \migration -> do
    migration config -- call the actual migration procedure
    update currentDBVersionId
      [ BlockTimeStrikeDBVersion +=. 1
      ]
    transactionSave -- keep each migration as one transaction to keep db at the latest successful migration
