{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GADTs                      #-}
module OpEnergy.BlockTimeStrike.Server.V2.BlockSpanTimeStrike
  ( apiBlockSpanTimeStrikeModelBlockTimeStrike
  ) where

import           Control.Monad.Trans.Reader ( asks)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Control.Monad.Trans
import           Control.Monad.Trans.Except( withExceptT, ExceptT (..))

import           Prometheus(MonadMonitor)
import           Servant ( err500, ServerError)


import qualified Data.OpEnergy.API.V1.Positive as APIV1
import qualified Data.OpEnergy.Account.API.V1.BlockTimeStrike
                 as APIV1
import qualified Data.OpEnergy.API.V1.Block
                 as APIV1
import qualified Data.OpEnergy.Client as BlockSpanClient

import qualified Data.OpEnergy.BlockTime.API.V2.BlockSpanTimeStrike as API

import qualified OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeGuess
                 as V1
import           OpEnergy.Error
                 ( runExceptPrefixT
                 , eitherException
                 )
import qualified OpEnergy.Account.Server.V1.Config as Config
import           OpEnergy.Account.Server.V1.Class
                 ( AppT, State(..),  profile )

apiBlockSpanTimeStrikeModelBlockTimeStrike
  :: ( MonadIO m
     , MonadMonitor m
     )
  => APIV1.BlockHeader
  -> APIV1.Positive Int
  -> APIV1.BlockTimeStrike
  -> V1.CalculatedBlockTimeStrikeGuessesCount
  -> AppT m (Either (ServerError, Text) API.BlockSpanTimeStrike)
apiBlockSpanTimeStrikeModelBlockTimeStrike confirmedBlock spanSize v guessesCount =
    let name = "apiBlockSpanTimeStrikeModelBlockTimeStrike"
    in profile name $ runExceptPrefixT name $ do
  mBlockSpan <- do
    if APIV1.blockTimeStrikeBlock v > APIV1.blockHeaderHeight confirmedBlock
      then return Nothing
      else do
        url <- lift $ asks $ Config.configBlockspanURL . config
        eBlockSpan <- withExceptT (\msg -> (err500, Text.pack (show msg))) $ ExceptT
          $ liftIO $ eitherException
            $ BlockSpanClient.withClientEither url $ do
              BlockSpanClient.v2getSingleBlockspan
                (APIV1.blockTimeStrikeBlock v)
                (Just spanSize)
        eret <- withExceptT (\msg -> (err500, Text.pack (show msg))) $ ExceptT
          $ return eBlockSpan
        return $! Just eret
  return $! API.BlockSpanTimeStrike
    { API.block = APIV1.blockTimeStrikeBlock v
    , API.mediantime = APIV1.blockTimeStrikeStrikeMediantime v
    , API.creationTime = APIV1.blockTimeStrikeCreationTime v
    , API.spanSize = spanSize
    , API.mBlockSpan = mBlockSpan
    , API.observedResult = APIV1.blockTimeStrikeObservedResult v
    , API.observedBlockMediantime = APIV1.blockTimeStrikeObservedBlockMediantime v
    , API.observedBlockHash = APIV1.blockTimeStrikeObservedBlockHash v
    , API.observedBlockHeight = APIV1.blockTimeStrikeObservedBlockHeight v
    , API.guessesCount =
      V1.calculatedBlockTimeStrikeGuessesCountGuessesCount guessesCount
    }

