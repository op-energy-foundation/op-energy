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


import qualified Data.OpEnergy.API.V1.Positive as APIV1
import qualified Data.OpEnergy.Account.API.V1.BlockTimeStrike
                 as APIV1
import qualified Data.OpEnergy.Client as BlockSpanClient

import qualified Data.OpEnergy.BlockTime.API.V2.BlockSpanTimeStrike as API

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
  => APIV1.Positive Int
  -> APIV1.BlockTimeStrike
  -> AppT m (Either Text API.BlockSpanTimeStrike)
apiBlockSpanTimeStrikeModelBlockTimeStrike spanSize v =
    let name = "apiBlockSpanTimeStrikeModelBlockTimeStrike"
    in profile name $ runExceptPrefixT name $ do
  mBlockSpan <- do
    case APIV1.blockTimeStrikeObservedResult v of
      Nothing -> return Nothing
      Just _ -> do
        url <- lift $ asks $ Config.configBlockspanURL . config
        eBlockSpan <- ExceptT
          $ liftIO $ eitherException
            $ BlockSpanClient.withClientEither url $ do
              BlockSpanClient.v2getSingleBlockspan
                (APIV1.blockTimeStrikeBlock v)
                (Just spanSize)
        eret <- withExceptT (Text.pack . show) $ ExceptT $ return eBlockSpan
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
    }

