{-- | This module implements BlockTime strike service.
 -}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GADTs                      #-}
module OpEnergy.BlockTimeStrike.Server.V2.BlockSpanTimeStrikeGuessService.StrikeGuesses
  ( getStrikeGuessesHandler
  ) where

import           Data.Text(Text)

import           Servant ( err500)
import           Control.Monad( forM)
import           Control.Monad.Trans.Reader ( asks)
import           Control.Monad.Logger( logError)
import           Control.Monad.Trans
import           Control.Monad.Trans.Except( ExceptT (..))

import           Data.OpEnergy.API.V1.Natural
import qualified Data.OpEnergy.API.V1.Positive as APIV1
import qualified Data.OpEnergy.API.V1.Block
                 as APIV1
import qualified Data.OpEnergy.Account.API.V1.PagingResult
                 as APIV1
import qualified Data.OpEnergy.Account.API.V1.FilterRequest
                 as APIV1

import           Data.OpEnergy.BlockTime.API.V2.BlockSpanTimeStrikeGuess
import qualified Data.OpEnergy.Account.API.V1.BlockTimeStrikeGuess
                 as BlockTimeStrikeGuessV1

import           OpEnergy.Error( eitherThrowJSON, runExceptPrefixT)
import qualified OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeGuessService
                 as V1
import qualified OpEnergy.Account.Server.V1.Config as Config
import           OpEnergy.Account.Server.V1.Class
                 ( AppM, State(..), runLogging, profile )
import qualified OpEnergy.BlockTimeStrike.Server.V2.BlockSpanTimeStrikeGuess
                 as BlockSpanTimeStrikeGuess

getStrikeGuessesHandler
  :: APIV1.BlockHeight
  -> Natural Int
  -> Maybe (Natural Int)
  -> Maybe
     ( APIV1.FilterRequest
       BlockTimeStrikeGuessV1.BlockTimeStrikeGuess
       BlockTimeStrikeGuessV1.BlockTimeStrikeGuessFilter
     )
  -> Maybe (APIV1.Positive Int)
  -> AppM (APIV1.PagingResult BlockSpanTimeStrikeGuess)
getStrikeGuessesHandler strikeBlockHeight strikeMediantime mpage mfilter
      mspanSize =
    let name = "BlockSpanTimeStrikeGuessService.StrikeGuesses.getStrikeGuessesHandler"
    in profile name $
  eitherThrowJSON
    ( \reason-> do
      callstack <- asks callStack
      runLogging $ $(logError) $ callstack <> ": " <> reason
      return (err500, reason)
    )
    $ getStrikeGuesses strikeBlockHeight strikeMediantime mpage mfilter
      mspanSize

getStrikeGuesses
  :: APIV1.BlockHeight
  -> Natural Int
  -> Maybe (Natural Int)
  -> Maybe
     ( APIV1.FilterRequest
       BlockTimeStrikeGuessV1.BlockTimeStrikeGuess
       BlockTimeStrikeGuessV1.BlockTimeStrikeGuessFilter
     )
  -> Maybe (APIV1.Positive Int)
  -> AppM (Either Text (APIV1.PagingResult BlockSpanTimeStrikeGuess))
getStrikeGuesses strikeBlockHeight strikeMediantime mpage mfilter mspanSize =
    let name = "getStrikeGuesses"
    in profile name $ runExceptPrefixT name $ do
  spanSize <- lift $ maybe
    (asks $ Config.configBlockSpanDefaultSize . config)
    pure
    mspanSize
  guessesV1 <- ExceptT $ V1.getBlockTimeStrikeGuessesPage strikeBlockHeight
    strikeMediantime mpage mfilter
  guessesV2 <- forM
    (APIV1.pagingResultResults guessesV1)
    ( ExceptT
    . BlockSpanTimeStrikeGuess.apiBlockSpanTimeStrikeGuessModelBlockTimeStrikeGuess
      spanSize
    )
  return $ APIV1.PagingResult
    { APIV1.pagingResultNextPage = APIV1.pagingResultNextPage guessesV1
    , APIV1.pagingResultResults = guessesV2
    }


