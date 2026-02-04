{-- | This module implements BlockTime strike service.
 -}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GADTs                      #-}
module OpEnergy.BlockTimeStrike.Server.V2.StrikesAPI.GetStrikes
  ( getStrikes
  , getStrikesHandler
  ) where

import           Data.Text(Text)
import           Servant ( err500, ServerError)
import           Control.Monad.Trans.Reader ( asks)
import           Control.Monad.Logger( logError)
import           Control.Monad(forM)
import           Control.Monad.Trans
import           Control.Monad.Trans.Except( runExceptT, ExceptT (..))
import           Data.Maybe( fromMaybe)
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TVar as TVar

import           Data.Conduit( (.|) )
import qualified Data.Conduit.List as C
import           Database.Persist.Pagination(SortOrder(..))
import           Database.Persist

import qualified Data.OpEnergy.API.V1.Natural as APIV1
import qualified Data.OpEnergy.API.V1.Positive as APIV1
import qualified Data.OpEnergy.Account.API.V1.BlockTimeStrike
                 as APIV1
import qualified Data.OpEnergy.Account.API.V1.PagingResult
                 as APIV1
import qualified Data.OpEnergy.Account.API.V1.FilterRequest
                 as APIV1

import           Data.OpEnergy.BlockTime.API.V2.BlockSpanTimeStrike

import           OpEnergy.Error( eitherThrowJSON, runExceptPrefixT)
import           OpEnergy.ExceptMaybe(exceptTMaybeT)
import qualified OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeService.GetBlockTimeStrikesPage
                 as V1
import qualified OpEnergy.Account.Server.V1.Config as Config
import           OpEnergy.Account.Server.V1.Class
                 ( AppM, State(..), runLogging, profile )
import qualified OpEnergy.BlockTimeStrike.Server.V1.Class as BlockTime
import qualified OpEnergy.BlockTimeStrike.Server.V2.BlockSpanTimeStrike
                 as BlockSpanTimeStrike
import qualified OpEnergy.PagingResult as PagingResult
import qualified OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeFilter
                 as BlockTimeStrikeFilter
import qualified OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrike
                 as V1
import qualified OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeGuess
                 as V1
import qualified OpEnergy.BlockTimeStrike.Server.V1.SlowFast as SlowFast

-- | this handler returns pageable result of the BlockSpanTimeStrikes for a
-- given spanSize, page and filter
getStrikesHandler
  :: Maybe (APIV1.Positive Int)
  -> Maybe (APIV1.Natural Int)
  -> Maybe ( APIV1.FilterRequest
             APIV1.BlockTimeStrike
             APIV1.BlockTimeStrikeFilter
           )
  -> AppM (APIV1.PagingResult BlockSpanTimeStrike)
getStrikesHandler mspanSize mpage mfilter =
    let name = "V2.getStrikesHandler"
    in profile name $ eitherThrowJSON
      (\reason-> do
        callstack <- asks callStack
        let msg = callstack <> ":" <> reason
        runLogging $ $(logError) msg
        return msg
      )
      $ getStrikes mspanSize mpage mfilter

data IsSortByGuessesCountNeeded
  = SortByGuessesCountNotNeeded
  | SortByGuessesCountNeeded

getStrikes
  :: Maybe (APIV1.Positive Int)
  -> Maybe (APIV1.Natural Int)
  -> Maybe ( APIV1.FilterRequest
             APIV1.BlockTimeStrike
             APIV1.BlockTimeStrikeFilter
           )
  -> AppM (Either (ServerError, Text) (APIV1.PagingResult BlockSpanTimeStrike))
getStrikes mspanSize mpage mfilterAPI =
    let name = "V2.getStrikes"
    in profile name $ runExceptPrefixT name $ do
  recordsPerReply <- lift $ asks (Config.configRecordsPerReply . config)
  let
      linesPerPage = maybe
        recordsPerReply
        ( fromMaybe recordsPerReply
        . APIV1.blockTimeStrikeFilterLinesPerPage
        . fst
        . APIV1.unFilterRequest
        )
        mfilter
  latestUnconfirmedBlockHeightV <- lift
    $ asks (BlockTime.latestUnconfirmedBlockHeight . blockTimeState)
  configBlockTimeStrikeGuessMinimumBlockAheadCurrentTip <- lift
    $ asks (Config.configBlockTimeStrikeGuessMinimumBlockAheadCurrentTip . config)
  latestConfirmedBlockV <- lift
    $ asks (BlockTime.latestConfirmedBlock . blockTimeState)
  (latestUnconfirmedBlockHeight, latestConfirmedBlock) <-
    ExceptT $ liftIO $ STM.atomically $ runExceptT $ (,)
      <$> (exceptTMaybeT ( err500, "latest unconfirmed block hasn't been received yet")
          $ TVar.readTVar latestUnconfirmedBlockHeightV
          )
      <*> (exceptTMaybeT ( err500, "latest confirmed block hasn't been received yet")
          $ TVar.readTVar latestConfirmedBlockV
          )
  let
      staticPartFilter = maybe
                           []
                           ( APIV1.buildFilter
                           . APIV1.unFilterRequest
                           . APIV1.mapFilter
                           )
                           mfilter
      strikeFilter =
        BlockTimeStrikeFilter.buildFilterByClass
          ( maybe
            Nothing
            ( APIV1.blockTimeStrikeFilterClass
            . fst
            . APIV1.unFilterRequest
            ) mfilter
          )
          latestUnconfirmedBlockHeight
          latestConfirmedBlock
          configBlockTimeStrikeGuessMinimumBlockAheadCurrentTip
        ++ staticPartFilter
  spanSize <- lift $ maybe
    (asks $ Config.configBlockSpanDefaultSize . config)
    pure
    mspanSize
  let
      eSortGuessesCount = case maybe
            APIV1.StrikeSortOrderDescend
            ( fromMaybe APIV1.StrikeSortOrderDescend
            . APIV1.blockTimeStrikeFilterSort
            . fst
            . APIV1.unFilterRequest
            )
            mfilter
          of
        APIV1.StrikeSortOrderAscend -> SortByGuessesCountNotNeeded
        APIV1.StrikeSortOrderDescend -> SortByGuessesCountNotNeeded
        APIV1.StrikeSortOrderAscendGuessesCount ->  SortByGuessesCountNeeded
        APIV1.StrikeSortOrderDescendGuessesCount -> SortByGuessesCountNeeded
  blockTimeStrikesGuesses <- exceptTMaybeT (err500, "DB query failed")
    $ case eSortGuessesCount of
      SortByGuessesCountNotNeeded-> PagingResult.pagingResult
        mpage
        linesPerPage
        strikeFilter
        sort
        V1.BlockTimeStrikeId -- select strikes with given filter first
        $  C.concatMapM V1.maybeFetchGuessesCount
        .| C.concatMapM (V1.maybeFetchObservedStrike mfilter)
        .| C.map (\(strikeE, mObserved, guessCount) ->
            ( renderBlockTimeStrike (strikeE, mObserved), guessCount))
      SortByGuessesCountNeeded -> PagingResult.pagingResult
        mpage
        linesPerPage
        []
        sort
        V1.CalculatedBlockTimeStrikeGuessesCountGuessesCount
        $  C.concatMapM (V1.fetchStrikeByGuessesCount strikeFilter)
        .| C.concatMapM (V1.maybeFetchObservedStrike mfilter)
        .| C.map (\(strikeE, mObserved, guessCount) ->
             ( renderBlockTimeStrike (strikeE, mObserved), guessCount))
  blockSpanTimeStrikes <- forM
        (APIV1.pagingResultResults blockTimeStrikesGuesses)
        ( \(strike, Entity _ guessesCount) -> ExceptT
          $ BlockSpanTimeStrike.apiBlockSpanTimeStrikeModelBlockTimeStrike
            latestConfirmedBlock
            spanSize
            strike
            guessesCount
        )
  return $! APIV1.PagingResult
    { APIV1.pagingResultNextPage =
      APIV1.pagingResultNextPage blockTimeStrikesGuesses
    , APIV1.pagingResultResults = blockSpanTimeStrikes
    }
  where
    mfilter = fmap V1.coerceFilterRequestBlockTimeStrike mfilterAPI
    sort = maybe
             Descend
             ( APIV1.sortOrder
             . APIV1.unFilterRequest
             )
             mfilter

renderBlockTimeStrike
  :: ( Entity V1.BlockTimeStrike
     , Maybe (Entity V1.BlockTimeStrikeObserved)
     )
  -> APIV1.BlockTimeStrike
renderBlockTimeStrike (Entity _ strike, mObserved) = APIV1.BlockTimeStrike
  { APIV1.blockTimeStrikeObservedResult = fmap
    (\(Entity _ v)-> SlowFast.apiModel $ V1.blockTimeStrikeObservedIsFast v)
    mObserved
  , APIV1.blockTimeStrikeObservedBlockMediantime = fmap
    (\(Entity _ v)-> V1.blockTimeStrikeObservedJudgementBlockMediantime v)
    mObserved
  , APIV1.blockTimeStrikeObservedBlockHash = fmap
    (\(Entity _ v)-> V1.blockTimeStrikeObservedJudgementBlockHash v)
    mObserved
  , APIV1.blockTimeStrikeObservedBlockHeight = fmap
    (\(Entity _ v)-> V1.blockTimeStrikeObservedJudgementBlockHeight v)
    mObserved
  , APIV1.blockTimeStrikeBlock = V1.blockTimeStrikeBlock strike
  , APIV1.blockTimeStrikeStrikeMediantime =
    V1.blockTimeStrikeStrikeMediantime strike
  , APIV1.blockTimeStrikeCreationTime =
    V1.blockTimeStrikeCreationTime strike
  }


