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
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE DerivingStrategies         #-}
module OpEnergy.BlockTimeStrike.Server.V1.DB.Migrations.CalculateBlockTimeStrikeGuessesCountSlowFast.BlockTimeStrike
  where

import           Data.Time.Clock.POSIX(POSIXTime)
import           GHC.Generics

import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.TH


import           Data.OpEnergy.API.V1.Block
import           Data.OpEnergy.Account.API.V1.BlockTimeStrikeGuess()

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

|]



