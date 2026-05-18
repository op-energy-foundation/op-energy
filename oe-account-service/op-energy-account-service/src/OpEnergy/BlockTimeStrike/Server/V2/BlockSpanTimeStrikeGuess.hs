{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GADTs                      #-}
module OpEnergy.BlockTimeStrike.Server.V2.BlockSpanTimeStrikeGuess
  ( apiBlockSpanTimeStrikeGuessModelBlockTimeStrikeGuess
  , apiBlockSpanTimeStrikeGuessModelBlockTimeStrikeGuess1
  ) where

import           Data.Text (Text)
import           Control.Monad.Trans
import           Control.Monad.Trans.Except( ExceptT (..))

import           Prometheus(MonadMonitor)
import           Servant ( ServerError)

import qualified Data.OpEnergy.API.V1.Positive as APIV1
import qualified Data.OpEnergy.Account.API.V1.BlockTimeStrikeGuess
                 as APIV1
import qualified Data.OpEnergy.API.V1.Block
                 as APIV1

import qualified Data.OpEnergy.BlockTime.API.V2.BlockSpanTimeStrikeGuess as API

import           OpEnergy.Error
                 ( runExceptPrefixT
                 )
import           OpEnergy.Account.Server.V1.Class ( AppT,  profile )
import qualified OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrike
                 as BlockTimeStrike
import qualified OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeGuess
                 as BlockTimeStrikeGuess
import qualified OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeGuess
                 as BlockSpanTimeStrikeGuess
import qualified OpEnergy.BlockTimeStrike.Server.V2.BlockSpanTimeStrike
                 as BlockSpanTimeStrike
import qualified OpEnergy.BlockTimeStrike.Server.V1.SlowFast as SlowFast

apiBlockSpanTimeStrikeGuessModelBlockTimeStrikeGuess
  :: ( MonadIO m
     , MonadMonitor m
     )
  => APIV1.BlockHeader
  -> APIV1.Positive Int
  -> APIV1.BlockTimeStrikeGuess
  -> BlockSpanTimeStrikeGuess.CalculatedBlockTimeStrikeGuessesCount
  -> AppT m (Either (ServerError, Text) API.BlockSpanTimeStrikeGuess)
apiBlockSpanTimeStrikeGuessModelBlockTimeStrikeGuess
  confirmedBlock spanSize v guessesCount =
    let name = "apiBlockSpanTimeStrikeGuessModelBlockTimeStrikeGuess"
    in profile name $ runExceptPrefixT name $ do
  spanStrike <- ExceptT $ BlockSpanTimeStrike.apiBlockSpanTimeStrikeModelBlockTimeStrike
    confirmedBlock spanSize (APIV1.strike v) guessesCount
  return $! API.BlockSpanTimeStrikeGuess
    { API.strike = spanStrike
    , API.creationTime = APIV1.creationTime v
    , API.guess = APIV1.guess v
    }

apiBlockSpanTimeStrikeGuessModelBlockTimeStrikeGuess1
  :: ( MonadIO m
     , MonadMonitor m
     )
  => APIV1.BlockHeader
  -> APIV1.Positive Int
  -> BlockTimeStrike.BlockTimeStrike
  -> Maybe BlockTimeStrike.BlockTimeStrikeObserved
  -> BlockSpanTimeStrikeGuess.BlockTimeStrikeGuess
  -> BlockSpanTimeStrikeGuess.CalculatedBlockTimeStrikeGuessesCount
  -> AppT m (Either (ServerError, Text) API.BlockSpanTimeStrikeGuess)
apiBlockSpanTimeStrikeGuessModelBlockTimeStrikeGuess1
  confirmedBlock spanSize strike observed guess guessesCount =
    let name = "apiBlockSpanTimeStrikeGuessModelBlockTimeStrikeGuess1"
    in profile name $ runExceptPrefixT name $ do
  let
      strikeAPI = BlockTimeStrike.apiModelBlockTimeStrike strike observed
  spanStrike <- ExceptT $ BlockSpanTimeStrike.apiBlockSpanTimeStrikeModelBlockTimeStrike
    confirmedBlock spanSize strikeAPI guessesCount
  return $! API.BlockSpanTimeStrikeGuess
    { API.strike = spanStrike
    , API.creationTime = BlockTimeStrikeGuess.blockTimeStrikeGuessCreationTime guess
    , API.guess = SlowFast.apiModel
      (BlockTimeStrikeGuess.blockTimeStrikeGuessIsFast guess)
    }

