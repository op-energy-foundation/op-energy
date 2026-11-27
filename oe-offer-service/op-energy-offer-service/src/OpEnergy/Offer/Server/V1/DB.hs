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
import           OpEnergy.Offer.Server.V1.PlatformStats(migratePlatformStats)
import           OpEnergy.Offer.Server.V1.TelegramLink(migrateTelegramLink)
import           OpEnergy.Offer.Server.V1.TelegramMessage(migrateTelegramMessage)

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
    -- creates platform_stats table, for both new and existing DBs
    printMigration migratePlatformStats
    runMigration migratePlatformStats

    -- creates the tables the telegram trade chat is mirrored into, for
    -- both new and existing DBs. No index is added beside them: the feed
    -- reads one group's messages newest first, which is the unique index
    -- on (chat_id, message_id) scanned backwards
    printMigration migrateTelegramMessage
    runMigration migrateTelegramMessage

    -- creates the table, which ties a telegram account to an op energy
    -- one, for both new and existing DBs
    printMigration migrateTelegramLink
    runMigration migrateTelegramLink

  -- the indexes of the contract table, built after the migration and
  -- outside its transaction.
  --
  -- CONCURRENTLY, because a plain CREATE INDEX holds a lock for as long as
  -- it takes to read the table, and every insert and update of a contract
  -- waits on it: on a table with a history behind it a restart of this
  -- service would stop an accept from being recorded until the build
  -- finished. CONCURRENTLY can not run inside a transaction block, which
  -- is what runSqlPoolNoTransaction is here for.
  --
  -- A build, which fails, leaves the index behind marked invalid, and
  -- IF NOT EXISTS then finds it and skips it. Clearing that is a DROP
  -- INDEX by hand, which is the price of not locking the table on a start
  liftIO $ runSqlPoolNoTransaction createContractIndexes pool Nothing

  return pool
  where
    connStr = TE.encodeUtf8
      $! "host=" <> configDBHost config
      <> " port=" <> (T.tshow $ configDBPort config)
      <> " user=" <> configDBUser config
      <> " dbname=" <> configDBName config
      <> " password=" <> configDBPassword config

-- | the indexes, which are not declared by a model: persistent creates
-- none for a foreign key, and none for the order a listing pages by.
--
-- Built for new and existing DBs alike, so a deployment, which has run
-- before, gains them on its next start.
createContractIndexes
  :: MonadIO m
  => ReaderT SqlBackend m ()
createContractIndexes = do
  -- the offer list reads the contracts of its offers by offer_id
  rawExecute
    "CREATE INDEX CONCURRENTLY IF NOT EXISTS contract_offer_id_idx\
    \ ON contract (offer_id)"
    []
  -- the contracts listing pages by target block, with the id breaking the
  -- tie of two contracts of the same one, so this is the order it reads.
  -- Ascending covers both directions the listing asks for -- a settled
  -- section reads the newest first -- as postgres reads an index backwards
  -- as readily as forwards. Without it each page is a sequential scan of
  -- the whole table and a sort, and so is the count beside it
  rawExecute
    "CREATE INDEX CONCURRENTLY IF NOT EXISTS\
    \ contract_target_block_id_idx ON contract (target_block, id)"
    []
  -- the same listing of one status only. The status is the leading column,
  -- so this index is read in order for that filter, which the index above
  -- can not do: ordered by target block it would have to walk and discard
  -- every contract of the other statuses
  rawExecute
    "CREATE INDEX CONCURRENTLY IF NOT EXISTS\
    \ contract_status_target_block_id_idx\
    \ ON contract (status, target_block, id)"
    []

offerDBMigrations :: [( Config -> ReaderT
                                  SqlBackend
                                  (Control.Monad.Logger.NoLoggingT
                                   (ResourceT IO)
                                  )
                                  ()
                                )
                               ]
offerDBMigrations =
  [ migration0_addOfferV2ColumnsAndContractTable
  ]

-- | Migration 0: add V2 columns to the offer table with defaults, and
-- let the ORM create the contract table. Existing rows get sensible
-- defaults (side=BEFORE, blockRate=10, totalContracts=1, matchedCount=0,
-- takerStakeSats = 100000 - makerStakeSats). Status values that moved
-- to ContractStatus (accepted, confirming, settled) are mapped to
-- OfferStatus.Filled.
migration0_addOfferV2ColumnsAndContractTable
  :: Config
  -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) ()
migration0_addOfferV2ColumnsAndContractTable _config = do
  -- add new columns with defaults so existing rows survive
  rawExecute "ALTER TABLE offer ADD COLUMN IF NOT EXISTS mtp_cutoff_epoch INT8 NOT NULL DEFAULT 0" []
  rawExecute "ALTER TABLE offer ADD COLUMN IF NOT EXISTS side VARCHAR NOT NULL DEFAULT 'BEFORE'" []
  rawExecute "ALTER TABLE offer ADD COLUMN IF NOT EXISTS taker_stake_sats INT8 NOT NULL DEFAULT 50000" []
  rawExecute "ALTER TABLE offer ADD COLUMN IF NOT EXISTS block_rate DOUBLE PRECISION NOT NULL DEFAULT 10.0" []
  rawExecute "ALTER TABLE offer ADD COLUMN IF NOT EXISTS total_contracts INT8 NOT NULL DEFAULT 1" []
  rawExecute "ALTER TABLE offer ADD COLUMN IF NOT EXISTS matched_count INT8 NOT NULL DEFAULT 0" []
  rawExecute "ALTER TABLE offer ADD COLUMN IF NOT EXISTS created_at_block INT8 NOT NULL DEFAULT 0" []
  -- set takerStakeSats = totalPot - makerStakeSats, clamped to [1, totalPot-1]
  rawExecute "UPDATE offer SET taker_stake_sats = GREATEST(1, 100000 - LEAST(maker_stake_sats, 99999))" []
  -- clamp any out-of-range maker stakes to [1, 99999]
  rawExecute "UPDATE offer SET maker_stake_sats = LEAST(GREATEST(maker_stake_sats, 1), 99999)" []
  -- map old statuses that now belong to ContractStatus
  rawExecute "UPDATE offer SET status = 'filled' WHERE status IN ('accepted', 'confirming', 'settled')" []
  transactionSave

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
