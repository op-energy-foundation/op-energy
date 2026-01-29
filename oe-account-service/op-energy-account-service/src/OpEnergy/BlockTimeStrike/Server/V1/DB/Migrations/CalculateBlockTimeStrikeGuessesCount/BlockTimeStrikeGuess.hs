{-- | This module implements BlockTime strike service.
 -}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DataKinds                  #-}
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
{-# LANGUAGE FlexibleContexts           #-}
module OpEnergy.BlockTimeStrike.Server.V1.DB.Migrations.CalculateBlockTimeStrikeGuessesCount.BlockTimeStrikeGuess
  where

import           Data.Time.Clock.POSIX(POSIXTime)
import           GHC.Generics

import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.TH


import           Data.OpEnergy.API.V1.Natural

import           OpEnergy.BlockTimeStrike.Server.V1.DB.Migrations.CalculateBlockTimeStrikeGuessesCount.BlockTimeStrike
import           OpEnergy.BlockTimeStrike.Server.V1.SlowFast (SlowFast)
import           OpEnergy.Account.Server.V1.Person

share [mkPersist sqlSettings, mkMigrate "mkCalculatedBlockTimeStrikeGuessesCount"] [persistLowerCase|
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



