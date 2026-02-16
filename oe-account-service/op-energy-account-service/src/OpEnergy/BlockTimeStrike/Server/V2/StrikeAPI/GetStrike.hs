{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE GADTs                      #-}
module OpEnergy.BlockTimeStrike.Server.V2.StrikeAPI.GetStrike
  ( getStrike
  , getStrikeHandler
  ) where

import           Data.Text(Text)
import           Control.Monad.Trans.Reader ( asks)
import           Control.Monad.Logger( logError)
import           Control.Monad.Trans
import           Control.Monad.Trans.Except( ExceptT (..))
import           Data.Maybe(fromMaybe)

import           Servant ( ServerError)

import           Data.OpEnergy.API.V1.Natural(Natural)
import           Data.OpEnergy.API.V1.Positive
import qualified Data.OpEnergy.API.V1.Block as BlockV1
import qualified Data.OpEnergy.Account.API.V1.BlockTimeStrike
                 as V1
import qualified Data.OpEnergy.BlockTime.API.V2.BlockSpanTimeStrike
                 as API

import           OpEnergy.Account.Server.V1.Class
                 ( AppM, State(..), runLogging, profile )

import           OpEnergy.Error( eitherThrowJSON, runExceptPrefixT)
import qualified OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeService
                 as V1
import qualified OpEnergy.BlockTimeStrike.Server.V2.BlockSpanTimeStrike
                 as BlockSpanTimeStrike
import qualified OpEnergy.Account.Server.V1.Config as Config

getStrikeHandler
  :: BlockV1.BlockHeight
  -> Natural Int
  -> Maybe (Positive Int)
  -> AppM API.BlockSpanTimeStrike
getStrikeHandler strikeBlock strikeMediantime mspanSize =
    let name = "V2.strikeAPI.GetStrike.getStrikeHandler"
    in profile name $ eitherThrowJSON
      (\ reason-> do
        callstack <- asks callStack
        let msg = callstack <> ":" <> reason
        runLogging $ $(logError) msg
        return msg
      )
      $ runExceptPrefixT name $ do
  ExceptT $ getStrike strikeBlock strikeMediantime mspanSize

getStrike
  :: BlockV1.BlockHeight
  -> Natural Int
  -> Maybe (Positive Int)
  -> AppM (Either (ServerError, Text) API.BlockSpanTimeStrike)
getStrike strikeBlock strikeMediantime mspanSize =
    let name = "getStrike"
    in profile name $ runExceptPrefixT name $ do
  strikeV1 <- ExceptT $ V1.getBlockTimeStrike strikeBlock strikeMediantime
  spanSize <- lift $ asks
    $ (`fromMaybe` mspanSize)
    . Config.configBlockSpanDefaultSize . config
  ExceptT $ BlockSpanTimeStrike.apiBlockSpanTimeStrikeModelBlockTimeStrike
    spanSize
    (V1.blockTimeStrikeWithGuessesCountStrike strikeV1)

