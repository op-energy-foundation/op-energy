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
  , transformBlockTimeStrikeGuessGuessFromTextToInt
  )where

import           Control.Monad.Trans.Reader(ReaderT)
import           Control.Monad.Logger    ( NoLoggingT )
import           Control.Monad(void)
import           Data.Text(Text)
import qualified Data.Text as Text
import           Control.Monad.Trans
import           Data.Time.Clock.POSIX( getPOSIXTime)
import           Data.Int(Int64)

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

-- |
-- ALTER TABLE "block_time_strike" DROP COLUMN "observed_result";
-- ALTER TABLE "block_time_strike" DROP COLUMN "observed_block_mediantime";
-- ALTER TABLE "block_time_strike" DROP COLUMN "observed_block_hash";
-- CREATe TABLE "block_time_strike_observed"("id" SERIAL8  PRIMARY KEY UNIQUE,"result" INT8 NOT NULL,"block_mediantime" INT8 NULL,"block_hash" VARCHAR NULL,"creation_time" INT8 NOT NULL,"strike" INT8 NOT NULL);
-- ALTER TABLE "block_time_strike_observed" ADD CONSTRAINT "unique_blocktime_strike_observation_strike" UNIQUE("strike");
-- ALTER TABLE "block_time_strike_observed" ADD CONSTRAINT "block_time_strike_observed_strike_fkey" FOREIGN KEY("strike") REFERENCES "block_time_strike"("id") ON DELETE RESTRICT  ON UPDATE RESTRICT;
-- ALTER TABLE "block_time_strike_guess" ALTER COLUMN "guess" TYPE INT8;

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
  C.runConduit
    $ streamEntities
      [ BlockTimeStrikeObservedResult1 !=. Nothing ]
      BlockTimeStrikeId
      (PageSize (fromPositive recordsPerReply))
      Ascend
      (Range Nothing Nothing)
    .| ( C.awaitForever $ \(Entity strikeId strike) -> do
         now <- liftIO getPOSIXTime
         case ( blockTimeStrikeObservedResult1 strike
              , do -- ensure that either both fields exist or none
                hash <- blockTimeStrikeObservedBlockHash1 strike
                mediantime <- blockTimeStrikeObservedBlockMediantime1 strike
                return (hash, mediantime)
              ) of
           (Nothing, _) -> return () -- do nothing
           (Just result, mObserved) -> do -- any result exist
             -- move result
             void $ lift $ insert $! BlockTimeStrikeResult
               { blockTimeStrikeResultResult = slowFastToInt result
               , blockTimeStrikeResultCreationTime = floor now
               , blockTimeStrikeResultStrike = strikeId
               }
             case mObserved of
               Nothing -> return ()
               Just (hash, mediantime) -> do -- if observed, then move into BlockTimeStrikeObserved
                 void $ lift $ insert $! BlockTimeStrikeObserved
                   { blockTimeStrikeObservedBlockMediantime = mediantime
                   , blockTimeStrikeObservedBlockHash = hash
                   , blockTimeStrikeObservedCreationTime = floor now
                   , blockTimeStrikeObservedStrike = strikeId
                   }
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

transformBlockTimeStrikeGuessGuessFromTextToInt
  :: Config
  -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) ()
transformBlockTimeStrikeGuessGuessFromTextToInt config = do
  -- 1. rename old column and create new one. For now it will be NULLable
  rawExecute (Text.unlines $
    [ "ALTER TABLE \"block_time_strike_guess\" RENAME COLUMN \"guess\" TO \"guess_text\";" -- rename old lfield
    , "ALTER TABLE \"block_time_strike_guess\" ADD COLUMN \"guess\" INT8 NULL;" -- we need it to be optional first
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
           [ BlockTimeStrikeGuessGuess =. (Just $! slowFastToInt $! blockTimeStrikeGuessGuessOld guess)
           ]
       )
    .| C.sinkNull
  -- 3. make it non nullable and remove old column
  rawExecute (Text.unlines $
    [ "ALTER TABLE \"block_time_strike_guess\" ALTER COLUMN \"guess\" TYPE INT8;" -- we need it to be optional first
    , "ALTER TABLE \"block_time_strike_guess\" DROP COLUMN \"guess_text\";" -- rename old lfield
    ]) []
  where
    recordsPerReply = configRecordsPerReply config

-- | convert text SlowFast into int
slowFastToInt :: Text-> Int64
slowFastToInt v
  | v == "slow" = 0
  | v == "fast" = 1
  | otherwise = error $ "slowFastToInt: unsupported value: " ++ show v
