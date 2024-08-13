{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE DerivingStrategies         #-}

module OpEnergy.BlockTimeStrike.Server.V1.DB.Migrations.SplitBlockTimeStrikeObservedFromBlockTimeStrike.Model
  where

import           GHC.Generics
import           Data.Text(Text)
import           Data.Word(Word64)

import           Database.Persist.TH
import           Database.Persist
import           Database.Persist.Postgresql

-- | here goes migration-local representations of the tables.
-- the reason why are exist is that we don't know the "actual" schema of the tables as we can
-- be not the last migration. The only thing we know are:
-- 1. previous schema of the block_time_strike table
-- 2. schema of the block_time_strike and block_time_strike_observed tables after migration
--
-- we can't use any our custom data type for a fields as we don't know if they stayed the same
-- or had been changed since.
--
-- at the same time, migration-only models are not have to have all the fields
-- for models that require no creation: only those fields, that are used in
-- migrations as the rest of the fields won't be touched during migrations
share [mkPersist sqlSettings] [persistLowerCase|
BlockTimeStrike
  -- in terms of migration, we care only about those fields
  -- data
  observedResult1 Text Maybe sql=observed_result
  observedBlockMediantime1 Word64 Maybe sql=observed_block_mediantime
  observedBlockHash1 Text Maybe sql=observed_block_hash
  -- metadata
  -- constraints
  deriving Eq Show Generic

BlockTimeStrikeGuess
  -- we care only about those 2 fields in terms of migration, so we don't care
  -- about any other field now
  -- data
  guessOld Text sql=guess_text
  guess Bool Maybe
  -- metadata
  -- constraints
  deriving Eq Show Generic

|]

-- | this model will be used for creation model needed for migration purposes
-- so it should contain all the fields, that should be compatible with the
-- main model definition as final migration will be applied at the end
share [mkPersist sqlSettings, mkMigrate "createBlockTimeStrikeObserved"] [persistLowerCase|
-- it expected, that this table will be created from migration above. So
-- we try to define it with basic types as we don't know the current state of
-- fields' types of this table
BlockTimeStrikeObserved
  -- data
  blockMediantime Word64 -- mediantime of the observed block. we decided to have this field redudancy
  blockHash Text -- hash of the observed block. BlockHash can be missing when strike observed by reaching desired strikeMediantime instead of observing block itself. In this case, block hash will be filled when block will be observed. we decided to have this field redudancy
  -- metadata
  creationTime Word64
  -- reflinks
  strike BlockTimeStrikeId
  -- constraints
  UniqueBlocktimeStrikeObservationStrike strike -- unique per strike
  deriving Eq Show Generic

-- this model should contain only strike outcome
BlockTimeStrikeResult
  -- data
  result Bool
  -- metadata
  creationTime Word64
  -- reflinks
  strike BlockTimeStrikeId
  -- constraints
  UniqueBlocktimeStrikeResultStrike strike -- unique per strike
  deriving Eq Show Generic
|]
