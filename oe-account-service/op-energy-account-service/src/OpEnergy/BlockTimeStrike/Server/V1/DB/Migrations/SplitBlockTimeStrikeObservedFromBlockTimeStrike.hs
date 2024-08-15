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

module OpEnergy.BlockTimeStrike.Server.V1.DB.Migrations.SplitBlockTimeStrikeObservedFromBlockTimeStrike
  ( splitBlockTimeStrikeObservedFromBlockTimeStrike
  , createBlockTimeStrikeObservedTable
  , transformBlockTimeStrikeGuessGuessFromTextToBool
  )where

import           Control.Monad.Trans.Reader(ReaderT)
import           Control.Monad.Logger    ( NoLoggingT )
import           Control.Monad(void)
import           Data.Text(Text)
import qualified Data.Text as Text
import           Control.Monad.Trans
import           Data.Time.Clock.POSIX( getPOSIXTime)

import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.Pagination
import           Data.OpEnergy.API.V1.Positive( fromPositive)
import           Control.Monad.Trans.Resource(ResourceT)
import           Data.Conduit( (.|) )
import qualified Data.Conduit as C
import qualified Data.Conduit.List as C

import           OpEnergy.Account.Server.V1.Config
import           OpEnergy.BlockTimeStrike.Server.V1.DB.Migrations.SplitBlockTimeStrikeObservedFromBlockTimeStrike.Model

createBlockTimeStrikeObservedTable
  :: Config
  -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) ()
createBlockTimeStrikeObservedTable _config = do
  runMigration createBlockTimeStrikeObserved

splitBlockTimeStrikeObservedFromBlockTimeStrike
  :: Config
  -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) ()
splitBlockTimeStrikeObservedFromBlockTimeStrike config = do
  -- 1. create block_time_strike_observed table
  runMigration createBlockTimeStrikeObserved
  -- 2. fill up new results table
  let isStrikeOutcomeAlreadyCalculated =
        BlockTimeStrikeObservedResult1 !=. Nothing
  C.runConduit
    $ streamEntities
      [ isStrikeOutcomeAlreadyCalculated ]
      BlockTimeStrikeId
      (PageSize (fromPositive recordsPerReply))
      Ascend
      (Range Nothing Nothing)
    .| ( C.awaitForever $ \(Entity strikeId strike) -> do
         case ( blockTimeStrikeObservedResult1 strike
              , do -- ensure that either both fields exist or none
                hash <- blockTimeStrikeObservedBlockHash1 strike
                mediantime <- blockTimeStrikeObservedBlockMediantime1 strike
                return (hash, mediantime)
              ) of
           (Just result, Just (judgementBlockHash, judgementBlockMediantime)) -> do -- any result exist
             -- move result
             now <- liftIO getPOSIXTime
             void $ lift $ insert $! BlockTimeStrikeObserved
               { blockTimeStrikeObservedIsFast = slowFastToBool result
               , blockTimeStrikeObservedCreationTime = floor now
               , blockTimeStrikeObservedStrike = strikeId
               , blockTimeStrikeObservedJudgementBlockMediantime =
                 judgementBlockMediantime
               , blockTimeStrikeObservedJudgementBlockHash =
                 judgementBlockHash
               , blockTimeStrikeObservedJudgementBlockHeight =
                 blockTimeStrikeBlock strike
               }
             return ()
           _ -> return () -- the rest will be recalculated by the main outcome calculation routine
       )
    .| C.sinkNull
  -- 3. remove old columns
  rawExecute (Text.unlines $
              [ "ALTER TABLE \"block_time_strike\" DROP COLUMN \"observed_result\";"
              , "ALTER TABLE \"block_time_strike\" DROP COLUMN \"observed_block_mediantime\";"
              , "ALTER TABLE \"block_time_strike\" DROP COLUMN \"observed_block_hash\";"
              ]
             ) []
  where
    recordsPerReply = configRecordsPerReply config

transformBlockTimeStrikeGuessGuessFromTextToBool
  :: Config
  -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) ()
transformBlockTimeStrikeGuessGuessFromTextToBool config = do
  -- 1. rename old column and create new one. For now it will be NULLable
  rawExecute (Text.unlines $
    [ "ALTER TABLE \"block_time_strike_guess\" ADD COLUMN \"is_fast\" BOOLEAN NULL;" -- we need it to be optional first
    ]) []

  -- 2. migrate the data
  C.runConduit
    $ streamEntities
      [ ]
      BlockTimeStrikeGuessId
      (PageSize (fromPositive recordsPerReply))
      Ascend
      (Range Nothing Nothing)
    .| ( C.awaitForever $ \(Entity guessId guess) -> do
         lift $ update guessId
           [ BlockTimeStrikeGuessIsFast =. (Just $! slowFastToBool $! blockTimeStrikeGuessGuess guess)
           ]
       )
    .| C.sinkNull
  -- 3. make it non nullable and remove old column
  rawExecute (Text.unlines $
    [ "ALTER TABLE \"block_time_strike_guess\" ALTER COLUMN \"is_fast\" TYPE BOOLEAN;" -- we need it to be optional first
    , "ALTER TABLE \"block_time_strike_guess\" DROP COLUMN \"guess\";" -- rename old lfield
    ]) []
  where
    recordsPerReply = configRecordsPerReply config

-- | convert text SlowFast into int
slowFastToBool :: Text-> Bool
slowFastToBool v
  | v == "slow" = False
  | v == "fast" = True
  | otherwise = error $ "slowFastToBool: unsupported value: " ++ show v
