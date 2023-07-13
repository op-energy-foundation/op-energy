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
module Data.OpEnergy.Account.API.V1.BlockTimeStrike where

import           Data.Swagger
import           Control.Lens
import           GHC.Generics
import           Data.Aeson
import           Data.Text                  (Text)
import           Data.Time.Clock.POSIX(POSIXTime)

import           Servant.API(ToHttpApiData(..), FromHttpApiData(..))
import           Database.Persist.TH
import           Database.Persist
import           Database.Persist.Sql

import           Data.OpEnergy.API.V1.Block(BlockHeight, defaultBlockHeight)
import           Data.OpEnergy.Account.API.V1.Account
import           Data.OpEnergy.API.V1.Block(BlockHash)
import qualified Data.OpEnergy.API.V1.Hash as BlockHash (defaultHash)

data GuessResult
  = Wrong
  | Right
  deriving (Eq, Show)


share [mkPersist sqlSettings, mkMigrate "migrateBlockTimeStrike"] [persistLowerCase|
BlockTimeStrikeFuture
  -- data
  block BlockHeight
  nlocktime POSIXTime 
  -- metadata
  creationTime POSIXTime
  -- constraints
  UniqueBlockTimeStrikeFutureBlockNLockTime block nlocktime
  deriving Eq Show Generic

BlockTimeStrikePast
  -- data
  block BlockHeight
  nlocktime POSIXTime
  observedResult SlowFast
  observedBlockMediantime POSIXTime
  observedBlockHash BlockHash
  -- metadata
  creationTime POSIXTime
  futureStrikeCreationTime  POSIXTime
  -- constraints
  UniqueBlockTimeStrikePastBlockNLockTime block nlocktime -- for now it is forbidden to have multiple strikes of the same (block,nlocktime) values
  deriving Eq Show Generic
|]

defaultBlockTimeStrikeFuture :: BlockTimeStrikeFuture
defaultBlockTimeStrikeFuture = BlockTimeStrikeFuture
  { blockTimeStrikeFutureBlock = defaultBlockHeight
  , blockTimeStrikeFutureNlocktime = defaultPOSIXTime
  , blockTimeStrikeFutureCreationTime = defaultPOSIXTime
  }
instance ToJSON BlockTimeStrikeFuture
instance ToSchema BlockTimeStrikeFuture where
  declareNamedSchema _ = return $ NamedSchema (Just "BlockTimeStrikeFuture") $ mempty
    & type_ ?~ SwaggerObject
    & example ?~ toJSON defaultBlockTimeStrikeFuture

instance ToJSON BlockTimeStrikePast
instance ToSchema BlockTimeStrikePast where
  declareNamedSchema _ = return $ NamedSchema (Just "BlockTimeStrikePast") $ mempty
    & type_ ?~ SwaggerObject
    & example ?~ toJSON defaultBlockTimeStrikePast
defaultBlockTimeStrikePast :: BlockTimeStrikePast
defaultBlockTimeStrikePast = BlockTimeStrikePast
  { blockTimeStrikePastBlock = defaultBlockHeight
  , blockTimeStrikePastNlocktime = defaultPOSIXTime
  , blockTimeStrikePastCreationTime = defaultPOSIXTime
  , blockTimeStrikePastFutureStrikeCreationTime = defaultPOSIXTime
  , blockTimeStrikePastObservedResult = defaultSlowFast
  , blockTimeStrikePastObservedBlockMediantime = defaultPOSIXTime
  , blockTimeStrikePastObservedBlockHash = BlockHash.defaultHash
  }


data SlowFast
  = Slow
  | Fast
  deriving (Eq, Enum, Show)

instance ToJSON SlowFast where
  toJSON Slow = toJSON ("slow" :: Text)
  toJSON Fast = toJSON ("fast" :: Text)
instance FromJSON SlowFast where
  parseJSON = withText "SlowFast" $! pure . verifySlowFast
instance PersistField SlowFast where
  toPersistValue Slow = toPersistValue ("slow"::Text)
  toPersistValue Fast = toPersistValue ("fast"::Text)
  fromPersistValue (PersistText "slow") = Prelude.Right $! Slow
  fromPersistValue (PersistText "fast") = Prelude.Right $! Fast
  fromPersistValue _ = Left $ "fromPersistValue SlowFastGuess, expected Text"
instance PersistFieldSql SlowFast where
  sqlType _ = SqlString

instance ToParamSchema SlowFast where
  toParamSchema _ = mempty
    & type_ ?~ SwaggerString
    & enum_ ?~ (map toJSON $ enumFrom Slow)
instance ToHttpApiData SlowFast where
  toUrlPiece Slow = "slow"
  toUrlPiece Fast = "fast"
instance FromHttpApiData SlowFast where
  parseUrlPiece "slow" = Prelude.Right Slow
  parseUrlPiece "fast" = Prelude.Right Slow
  parseUrlPiece _ = Left "wrong SlowFast value"
  
defaultSlowFast :: SlowFast
defaultSlowFast = Slow

verifySlowFast :: Text-> SlowFast
verifySlowFast "slow" = Slow
verifySlowFast "fast" = Fast
verifySlowFast _ = error "verifySlowFast: wrong value"
