{-- | This module implements BlockTime strike service.
 -}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GADTs                      #-}
module OpEnergy.BlockTimeStrike.Server.V2.BlockSpanTimeStrikeGuessService.GetStrikesGuesses
  ( getStrikesGuesses
  , getStrikesGuessesHandler
  ) where

import           Data.Text(Text)

import           Servant ( err500)
import           Control.Monad.Trans.Reader ( asks)
import           Control.Monad.Logger( logError)
import           Control.Monad(forM)
import           Control.Monad.Trans
import           Control.Monad.Trans.Except( ExceptT (..))

import qualified Data.OpEnergy.API.V1.Natural as APIV1
import qualified Data.OpEnergy.API.V1.Positive as APIV1
import qualified Data.OpEnergy.Account.API.V1.BlockTimeStrikeGuess
                 as BlockTimeStrikeGuessV1
import qualified Data.OpEnergy.Account.API.V1.PagingResult
                 as APIV1
import qualified Data.OpEnergy.Account.API.V1.FilterRequest
                 as APIV1

import           Data.OpEnergy.BlockTime.API.V2.BlockSpanTimeStrikeGuess

import           OpEnergy.Error( eitherThrowJSON, runExceptPrefixT)
import qualified OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeGuessService
                 as V1
import qualified OpEnergy.Account.Server.V1.Config as Config
import           OpEnergy.Account.Server.V1.Class
                 ( AppM, State(..), runLogging, profile )
import qualified OpEnergy.BlockTimeStrike.Server.V2.BlockSpanTimeStrikeGuess
                 as BlockSpanTimeStrikeGuess

getStrikesGuessesHandler
  :: Maybe (APIV1.Positive Int)
  -> Maybe (APIV1.Natural Int)
  -> Maybe ( APIV1.FilterRequest
             BlockTimeStrikeGuessV1.BlockTimeStrikeGuess
             BlockTimeStrikeGuessV1.BlockTimeStrikeGuessFilter
           )
  -> AppM (APIV1.PagingResult BlockSpanTimeStrikeGuess)
getStrikesGuessesHandler mspanSize mpage mfilter =
    let name = "V2.getStrikesGuessesHandler"
    in profile name $ eitherThrowJSON
      (\reason-> do
        callstack <- asks callStack
        runLogging $ $(logError) $ callstack <> ":" <> reason
        return (err500, reason)
      )
      $ getStrikesGuesses mspanSize mpage mfilter

getStrikesGuesses
  :: Maybe (APIV1.Positive Int)
  -> Maybe (APIV1.Natural Int)
  -> Maybe ( APIV1.FilterRequest
             BlockTimeStrikeGuessV1.BlockTimeStrikeGuess
             BlockTimeStrikeGuessV1.BlockTimeStrikeGuessFilter
           )
  -> AppM (Either Text (APIV1.PagingResult BlockSpanTimeStrikeGuess))
getStrikesGuesses mspanSize mpage mfilter =
    let name = "V2.getStrikesGuesses"
    in profile name $ runExceptPrefixT name $ do
  guessesV1 <- ExceptT $ V1.getBlockTimeStrikesGuessesPage mpage mfilter
  spanSize <- lift $ maybe
    (asks $ Config.configBlockSpanDefaultSize . config)
    pure
    mspanSize
  guessesV2 <- forM
    (APIV1.pagingResultResults guessesV1)
    ( ExceptT
    . BlockSpanTimeStrikeGuess.apiBlockSpanTimeStrikeGuessModelBlockTimeStrikeGuess
      spanSize
    )
  return $! APIV1.PagingResult
    { APIV1.pagingResultNextPage = APIV1.pagingResultNextPage guessesV1
    , APIV1.pagingResultResults = guessesV2
    }

