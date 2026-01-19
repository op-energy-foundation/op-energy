{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE GADTs                      #-}
module OpEnergy.BlockTimeStrike.Server.V2.StrikeGuessesAPI.Summary
  ( get
  ) where

import           Servant

import           Control.Monad.Trans.Reader ( asks)
import           Control.Monad.Logger( logError)
import           Control.Monad.Trans.Except
                   ( throwE)

import           Data.OpEnergy.API.V1.Natural(Natural)
import qualified Data.OpEnergy.API.V1.Block as BlockV1

import           OpEnergy.Account.Server.V1.Class
                   ( AppM, runLogging, profile, State(..) )

import qualified Data.OpEnergy.BlockTime.API.V2.BlockSpanTimeStrikeGuessesSummary
                 as API
import           OpEnergy.Error

get
  :: BlockV1.BlockHeight
  -> Natural Int
  -> AppM API.BlockSpanTimeStrikeGuessesSummary
get block mediantime =
    let name = "V2.StrikeGuessesAPI.Summary.get"
    in profile name $ eitherThrowJSON
      (\reason-> do
        callstack <- asks callStack
        runLogging $ $(logError) $ callstack <> ":" <> reason
        return (err500, reason)
      )
      $ runExceptPrefixT name  $ do
  let
      _ = undefined block mediantime
  throwE "not implemented"


