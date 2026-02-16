{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE GADTs                      #-}
module OpEnergy.BlockTimeStrike.Server.V2.StrikeGuessesAPI.Summary
  ( get
  ) where

import           Servant

import           Control.Monad.Trans.Reader ( asks)
import           Control.Monad.Logger( logError, logInfo)
import           Control.Monad.Trans(lift)
import           Control.Monad.Trans.Maybe
                   ( MaybeT(..), runMaybeT)
import           Data.Text.Show (tshow)

import           Database.Persist((==.))
import qualified Database.Persist as Persist

import           Data.OpEnergy.API.V1.Natural(Natural)
import qualified Data.OpEnergy.API.V1.Block as BlockV1

import qualified Data.OpEnergy.BlockTime.API.V2.BlockSpanTimeStrikeGuessesSummary
                 as API

import           OpEnergy.Account.Server.V1.Class
                   ( AppM, runLogging, profile, State(..)
                   , withDBTransaction
                   )
import qualified OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrike
                 as V1
import qualified OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeGuess
                 as V1
import           OpEnergy.Error
import           OpEnergy.ExceptMaybe

get
  :: BlockV1.BlockHeight
  -> Natural Int
  -> AppM API.BlockSpanTimeStrikeGuessesSummary
get strikeBlock strikeMediantime =
    let name = "V2.StrikeGuessesAPI.Summary.get"
    in profile name $ eitherThrowJSON
      (\reason-> do
        callstack <- asks callStack
        let msg = callstack <> ":" <> reason
        runLogging $ $(logError) msg
        return msg
      )
      $ runExceptPrefixT name  $ do
  lift $ runLogging $ $(logInfo) $! name <> ": " <> tshow (strikeBlock, strikeMediantime)
  mGuessesCount <- exceptTMaybeT (err500, "DB query failed")
    $ withDBTransaction "CalculatedBlockTimeStrikeGuessesCount" $ runMaybeT $ do
      Persist.Entity strikeId _ <- MaybeT $ Persist.selectFirst
        [ V1.BlockTimeStrikeBlock ==. strikeBlock
        , V1.BlockTimeStrikeStrikeMediantime ==. fromIntegral strikeMediantime
        ]
        []
      MaybeT $ Persist.selectFirst
        [ V1.CalculatedBlockTimeStrikeGuessesCountStrike ==. strikeId
        ]
        []
  Persist.Entity _ guessesCount <- exceptTMaybeT(err400, "strike not found")
    $ return mGuessesCount
  return $! API.BlockSpanTimeStrikeGuessesSummary
    { API.slowCount =
      V1.calculatedBlockTimeStrikeGuessesCountSlowCount guessesCount
    , API.fastCount =
      V1.calculatedBlockTimeStrikeGuessesCountFastCount guessesCount
    }



