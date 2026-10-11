{-- | This module defines the PlatformStats Persistent entity: a single-row
 - table which tracks the platform fees collected from settled contracts.
 - Fees are burned (not credited to any account), so this table is the only
 - place where they are visible.
 -}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE EmptyDataDecls             #-}
module OpEnergy.Offer.Server.V1.PlatformStats
  where

import           Data.Word(Word64)
import           Data.Time.Clock(UTCTime)
import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Trans.Reader(ReaderT)

import           Database.Persist
import           Database.Persist.TH
import           Database.Persist.Sql(SqlBackend)

share [mkPersist sqlSettings, mkMigrate "migratePlatformStats"] [persistLowerCase|

-- | this table contains only one record with the platform-wide totals
PlatformStats
  totalFeesCollectedSats Word64
  lastUpdated UTCTime
  deriving Eq Show

|]

-- | adds the given fee to the platform's total of collected fees, creating
-- the single record if it does not exist yet. Should be called within the
-- same transaction as the settlement of the contract which produced the fee.
-- The select-then-insert is safe only because the settlement sweep, running
-- in the single scheduler thread, is the only writer of this table.
addCollectedFeeTx
  :: MonadIO m
  => Word64
  -> UTCTime
  -> ReaderT SqlBackend m ()
addCollectedFeeTx feeSats now = do
  mstats <- selectFirst [] []
  case mstats of
    Nothing -> insert_ $! PlatformStats
      { platformStatsTotalFeesCollectedSats = feeSats
      , platformStatsLastUpdated = now
      }
    Just (Entity statsId _) -> update statsId
      [ PlatformStatsTotalFeesCollectedSats +=. feeSats
      , PlatformStatsLastUpdated =. now
      ]
