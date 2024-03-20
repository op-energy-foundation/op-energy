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
module Data.OpEnergy.Account.API.V1.BlockTimeStrikeGuess where

import           Data.Swagger
import           Control.Lens
import           GHC.Generics
import           Data.Aeson
import           Data.Time.Clock.POSIX(POSIXTime)

import           Database.Persist.TH
import           Database.Persist
import           Database.Persist.Sql

import           Data.OpEnergy.Account.API.V1.Account
import           Data.OpEnergy.Account.API.V1.BlockTimeStrike
import           Data.OpEnergy.Account.API.V1.UUID

share [mkPersist sqlSettings, mkMigrate "migrateBlockTimeStrikeGuess"] [persistLowerCase|
BlockTimeStrikeFutureGuess
  -- data
  guess SlowFast
  -- metadata
  creationTime POSIXTime
  -- reflinks
  person PersonId
  strike BlockTimeStrikeFutureId
  -- constraints
  UniquePersonStrikeGuess person strike -- only 1 guess per strike is allowed for person
  deriving Eq Show Generic

BlockTimeStrikePastGuess
  -- data
  guess SlowFast
  observedResult SlowFast
  -- metadata
  creationTime POSIXTime
  futureGuessCreationTime POSIXTime
  -- reflinks
  strike BlockTimeStrikePastId
  person PersonId
  -- constraints
  UniquePersonStrikeGuessResult person strike -- only 1 guess per strike is allowed for person
  deriving Eq Show Generic
|]

data BlockTimeStrikeGuessPublic = BlockTimeStrikeGuessPublic
  { person :: UUID Person
  , strike ::  BlockTimeStrikeFuture
  , creationTime :: POSIXTime
  , guess :: SlowFast
  }
  deriving (Eq, Show, Generic)
instance ToJSON BlockTimeStrikeGuessPublic
instance ToSchema BlockTimeStrikeGuessPublic where
  declareNamedSchema _ = return $ NamedSchema (Just "BlockTimeStrikeGuessPublic") $ mempty
    & type_ ?~ SwaggerObject
    & example ?~ toJSON defaultBlockTimeStrikeGuessPublic

defaultBlockTimeStrikeGuessPublic :: BlockTimeStrikeGuessPublic
defaultBlockTimeStrikeGuessPublic = BlockTimeStrikeGuessPublic
  { person = defaultUUID
  , strike = defaultBlockTimeStrikeFuture
  , creationTime = defaultPOSIXTime
  , guess = defaultSlowFast
  }

data BlockTimeStrikeGuessResultPublic = BlockTimeStrikeGuessResultPublic
  { person :: UUID Person
  , strike :: BlockTimeStrikePast
  , creationTime :: POSIXTime
  , archiveTime :: POSIXTime
  , guess :: SlowFast
  , observedResult :: SlowFast
  }
  deriving (Eq, Show, Generic)
instance ToJSON BlockTimeStrikeGuessResultPublic
instance ToSchema BlockTimeStrikeGuessResultPublic where
  declareNamedSchema _ = return $ NamedSchema (Just "BlockTimeStrikeGuessResultPublic") $ mempty
    & type_ ?~ SwaggerObject
    & example ?~ toJSON defaultBlockTimeStrikeGuessResultPublic
defaultBlockTimeStrikeGuessResultPublic :: BlockTimeStrikeGuessResultPublic
defaultBlockTimeStrikeGuessResultPublic = BlockTimeStrikeGuessResultPublic
  { person = defaultUUID
  , strike = defaultBlockTimeStrikePast
  , creationTime = defaultPOSIXTime
  , archiveTime = defaultPOSIXTime
  , guess = defaultSlowFast
  , observedResult = defaultSlowFast
  }
