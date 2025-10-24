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

module OpEnergy.BlockTimeStrike.Server.V1.DB.Migrations.CalculateBlockTimeStrikeCalculateGuessesCount
  ( migration
  ) where

import           Control.Monad.Trans.Reader(ReaderT)
import           Control.Monad.Logger    ( NoLoggingT )
import           Control.Monad(void)

import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.Pagination
import           Control.Monad.Trans.Resource(ResourceT)
import           Data.Conduit( (.|) )
import qualified Data.Conduit as C
import qualified Data.Conduit.List as C

import           Data.OpEnergy.API.V1.Natural

import qualified OpEnergy.Account.Server.V1.Config as Config
import qualified OpEnergy.PagingResult as PagingResult
import qualified OpEnergy.BlockTimeStrike.Server.V1.DB.Migrations.BlockTimeStrikeToBlockSpanTimeStrike.ModelBefore
                 as DB

-- | this migration perform calculation of guesses count for existing strike in order to
-- fill appropriate CalculatedBlockTimeStrikeGuessesCount table
migration
  :: Config.Config
  -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) ()
migration config = do
      C.runConduit
        $ PagingResult.pagingLoopSource -- search for strikes
          Nothing
          recordsPerReply
          []
          Descend
          DB.BlockTimeStrikeId
        .| C.mapM calculateGuessesCountForStrike
        .| C.sinkNull
      where
        recordsPerReply = Config.configRecordsPerReply config
        calculateGuessesCountForStrike (Entity strikeId _) = do
          guessesCount <- verifyNatural <$>
            count [ DB.BlockTimeStrikeGuessStrike ==. strikeId ] -- count guesses
          mguessesRecord <- selectFirst
            [ DB.CalculatedBlockTimeStrikeGuessesCountStrike ==. strikeId
            ]
            []
          case mguessesRecord of
            Nothing -> do -- insert new row for appropriate strike
              void $ insert $ DB.CalculatedBlockTimeStrikeGuessesCount
                { calculatedBlockTimeStrikeGuessesCountGuessesCount = guessesCount
                , calculatedBlockTimeStrikeGuessesCountStrike = strikeId
                }
            Just (Entity guessesRecordId _)-> do -- update existing strike
              update guessesRecordId
                [ DB.CalculatedBlockTimeStrikeGuessesCountGuessesCount =. guessesCount
                ]
