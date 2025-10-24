
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

module OpEnergy.BlockTimeStrike.Server.V1.DB.Migrations.BlockTimeStrikeToBlockSpanTimeStrike.ModelBefore
  where

import           GHC.Generics

import           Database.Persist.TH
import           Database.Persist
import           Database.Persist.Postgresql

import           Data.Time.Clock.POSIX(POSIXTime)

import           Data.OpEnergy.API.V1.Natural
import           Data.OpEnergy.API.V1.Block
import           OpEnergy.Account.Server.V1.Person
import           OpEnergy.BlockTimeStrike.Server.V1.SlowFast(SlowFast)


-- | here goes migration-local representations of the tables.
share [mkPersist sqlSettings] [persistLowerCase|
BlockTimeStrike
  -- data
  block BlockHeight
  strikeMediantime POSIXTime
  -- metadata
  creationTime POSIXTime
  -- constraints
  UniqueBlockTimeStrikeBlockStrikeMediantime block strikeMediantime -- for now it is forbidden to have multiple strikes of the same (block,strikeMediantime) values
  deriving Eq Show Generic

BlockTimeStrikeObserved
  -- data
  -- those fields are being kept as a sanity check in case of block chain
  -- reorganization as a last prove of the outcome. Though, we use confirmation
  -- algorithm, which goal is to reduce a possibility of hitting this case
  judgementBlockMediantime POSIXTime -- mediantime of the judgement block.
  judgementBlockHash BlockHash -- hash of the judgement block.
  judgementBlockHeight BlockHeight -- height of the judgement block.
  isFast SlowFast
  -- metadata
  creationTime POSIXTime
  -- reflinks
  strike BlockTimeStrikeId
  -- constraints
  UniqueBlocktimeStrikeObservationStrike strike -- unique per strike
  deriving Eq Show Generic

BlockTimeStrikeGuess
  -- data
  isFast SlowFast
  -- metadata
  creationTime POSIXTime
  -- reflinks
  strike BlockTimeStrikeId
  person PersonId
  -- constraints
  UniqueBlockTimeStrikeGuessPersonStrike person strike -- only 1 guess per strike is allowed for person
  deriving Eq Show Generic

-- this table's goal is to contain precalculated guesses count for a given strike. The reason
-- it exists is to eliminate a need of walking through the db in order to return a result
-- sorted by guesses count
CalculatedBlockTimeStrikeGuessesCount
  strike BlockTimeStrikeId
  guessesCount (Natural Int)
  UniqueCalculatedBlockTimeStrikeGuessesCountStrike strike -- allow only one record per strike
  deriving Eq Show Generic

|]

