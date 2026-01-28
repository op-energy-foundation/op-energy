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
module OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeGuess
  where

import           Data.Time.Clock.POSIX(POSIXTime)
import           GHC.Generics
import qualified Data.List as List
import           Data.Maybe( fromMaybe)
import           Data.Proxy

import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Pagination
import           Database.Persist.TH


import           Data.OpEnergy.API.V1.Natural
import qualified Data.OpEnergy.Account.API.V1.BlockTimeStrikeGuess as API
import qualified Data.OpEnergy.Account.API.V1.FilterRequest as API

import           OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrike
import           OpEnergy.BlockTimeStrike.Server.V1.SlowFast (SlowFast)
import qualified OpEnergy.BlockTimeStrike.Server.V1.SlowFast as SlowFast
import           OpEnergy.Account.Server.V1.Person

share [mkPersist sqlSettings, mkMigrate "migrateBlockTimeStrikeGuess"] [persistLowerCase|
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

instance API.BuildFilter BlockTimeStrikeGuess API.BlockTimeStrikeGuessFilter where
  sortOrder (filter, _) = fromMaybe Descend (API.blockTimeStrikeGuessFilterSort filter)
  buildFilter ( API.BlockTimeStrikeGuessFilter
                _
                _
                -- guess
                mGuessEQ
                mGuessNEQ
                -- observed
                _
                _
                -- strike block height
                _
                _
                _
                _
                -- strike strikeMediantime
                _
                _
                _
                _
                -- sort
                _
                _
                _ -- lines per page
              , _
              ) = List.concat
    [ maybe
      []
      (\v-> [ BlockTimeStrikeGuessIsFast ==. SlowFast.modelApi v])
      mGuessEQ
    , maybe
      []
      (\v-> [BlockTimeStrikeGuessIsFast !=. SlowFast.modelApi v])
      mGuessNEQ
    ]

coerceFilterRequestBlockTimeStrikeGuess
  :: API.BuildFilter BlockTimeStrikeGuess a
  => API.FilterRequest API.BlockTimeStrikeGuess a
  -> API.FilterRequest BlockTimeStrikeGuess a
coerceFilterRequestBlockTimeStrikeGuess = API.FilterRequest
  . (\(f, _)-> (f, Proxy))
  . API.unFilterRequest

