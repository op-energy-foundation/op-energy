{-- | This module implements BlockTime strike service.
 -}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GADTs                      #-}
module OpEnergy.BlockTimeStrike.Server.V2.StrikesAPI.GetStrikesGuesses
  ( getStrikesGuesses
  , getStrikesGuessesHandler
  ) where

import           Data.Text(Text)
import           Servant ( err400, err500, ServerError)
import           Control.Monad.Trans.Reader ( asks)
import           Control.Monad.Logger( NoLoggingT, logError)
import           Control.Monad(forM)
import           Control.Monad.Reader(ReaderT)
import           Control.Monad.Trans
import           Control.Monad.Trans.Except( ExceptT (..))
import           Data.Maybe( fromMaybe)

import           Data.Proxy(Proxy(..))
import           Data.Conduit( (.|) )
import qualified Data.Conduit.List as C
import           Database.Persist.Pagination(SortOrder(..))
import           Database.Persist
import           Database.Persist.Sql
import           Control.Monad.Trans.Resource( ResourceT)

import qualified Data.OpEnergy.API.V1.Natural as APIV1
import qualified Data.OpEnergy.API.V1.Positive as APIV1
import qualified Data.OpEnergy.Account.API.V1.Account
                 as AccountV1
import qualified Data.OpEnergy.Account.API.V1.PagingResult
                 as APIV1
import qualified Data.OpEnergy.Account.API.V1.FilterRequest
                 as APIV1

import           Data.OpEnergy.BlockTime.API.V2.BlockSpanTimeStrikeGuess
import           Data.OpEnergy.BlockTime.API.V2.BlockSpanTimeStrikeGuessFilter
                 as BlockSpanTimeStrikeGuessFilter

import           OpEnergy.Error( eitherThrowJSON, runExceptPrefixT)
import           OpEnergy.ExceptMaybe(exceptTMaybeT)
import qualified OpEnergy.Account.Server.V1.Config as Config
import           OpEnergy.Account.Server.V1.Class
                 ( AppM, State(..), runLogging, profile, getCurrentHeaderTip)
import qualified OpEnergy.Account.Server.V1.AccountService
                 as V1
import qualified OpEnergy.PagingResult as PagingResult
import qualified OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeFilter
                 as BlockTimeStrikeFilter
import qualified OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrike
                 as V1
import qualified OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeGuess
                 as V1
import qualified OpEnergy.BlockTimeStrike.Server.V2.BlockSpanTimeStrikeGuess
                 as BlockSpanTimeStrikeGuess
import           OpEnergy.BlockTimeStrike.Server.V2.BlockSpanTimeStrikeGuess.BlockSpanTimeStrikeGuessFilter
                 ()

-- | this handler returns pageable result of the BlockSpanTimeStrikes for a
-- given spanSize, page and filter
getStrikesGuessesHandler
  :: AccountV1.AccountToken
  -> Maybe (APIV1.Positive Int)
  -> Maybe (APIV1.Natural Int)
  -> Maybe ( APIV1.FilterRequest
             BlockSpanTimeStrikeGuess
             BlockSpanTimeStrikeGuessFilter
           )
  -> AppM (APIV1.PagingResult BlockSpanTimeStrikeGuess)
getStrikesGuessesHandler token mspanSize mpage mfilter =
    let name = "V2.getStrikesGuessesHandler"
    in profile name $ eitherThrowJSON
      (\reason-> do
        callstack <- asks callStack
        let msg = callstack <> ":" <> reason
        runLogging $ $(logError) msg
        return msg
      )
      $ getStrikesGuesses token mspanSize mpage mfilter

getStrikesGuesses
  :: AccountV1.AccountToken
  -> Maybe (APIV1.Positive Int)
  -> Maybe (APIV1.Natural Int)
  -> Maybe ( APIV1.FilterRequest
             BlockSpanTimeStrikeGuess
             BlockSpanTimeStrikeGuessFilter
           )
  -> AppM (Either (ServerError, Text) (APIV1.PagingResult BlockSpanTimeStrikeGuess))
getStrikesGuesses token mspanSize mpage mfilterAPI =
    let name = "V2.getStrikesGuesses"
    in profile name $ runExceptPrefixT name $ do
  Entity personKey _ <- exceptTMaybeT
    (err400, "person was not able to authenticate itself")
    $ V1.mgetPersonByAccountToken token
  recordsPerReply <- lift $ asks (Config.configRecordsPerReply . config)
  let
      linesPerPage = maybe
        recordsPerReply
        ( fromMaybe recordsPerReply
        . BlockSpanTimeStrikeGuessFilter.linesPerPage
        . fst
        . APIV1.unFilterRequest
        )
        mfilterGuess
  configBlockTimeStrikeGuessMinimumBlockAheadCurrentTip <- lift
    $ asks (Config.configBlockTimeStrikeGuessMinimumBlockAheadCurrentTip . config)
  (latestUnconfirmedBlockHeight, latestConfirmedBlock) <-
    ExceptT getCurrentHeaderTip
  let
      strikeFilter =
        BlockTimeStrikeFilter.buildFilterByClass
          ( maybe
            Nothing
            ( BlockSpanTimeStrikeGuessFilter._class
            . fst
            . APIV1.unFilterRequest
            ) mfilter
          )
          latestUnconfirmedBlockHeight
          latestConfirmedBlock
          configBlockTimeStrikeGuessMinimumBlockAheadCurrentTip
        ++ strikeStaticPartFilter
      guessFilter =
        ( V1.BlockTimeStrikeGuessPerson ==. personKey ) : guessStaticPartFilter
  spanSize <- lift $ maybe
    (asks $ Config.configBlockSpanDefaultSize . config)
    pure
    mspanSize
  blockTimeStrikesGuesses <- exceptTMaybeT (err500, "DB query failed")
    $ PagingResult.pagingResult
        mpage
        linesPerPage
        guessFilter
        sort
        V1.BlockTimeStrikeGuessId -- select strikes with given filter first
        $  C.concatMapM (maybeFetchBlockTimeStrikeByGuess strikeFilter)
        .| C.concatMapM (fetchBlockTimeStrikeObservedByStrike observedStaticFilter)
        .| C.concatMapM maybeFetchCalculatedGuessesCount
  blockSpanTimeStrikesGuessesApi <- forM
        (APIV1.pagingResultResults blockTimeStrikesGuesses)
        (   \(Entity _ guess
             , Entity _ strike
             , mObserved
             , Entity _ guessesCount
             ) -> do
          ExceptT
            $ BlockSpanTimeStrikeGuess.apiBlockSpanTimeStrikeGuessModelBlockTimeStrikeGuess1
              latestConfirmedBlock
              spanSize
              strike
              (fmap (\(Entity _ observed) -> observed) mObserved)
              guess
              guessesCount
        )
  return $! APIV1.PagingResult
    { APIV1.pagingResultNextPage =
      APIV1.pagingResultNextPage blockTimeStrikesGuesses
    , APIV1.pagingResultResults = blockSpanTimeStrikesGuessesApi
    }
  where
    maybeFetchBlockTimeStrikeByGuess
      :: [Filter V1.BlockTimeStrike]
      -> Entity V1.BlockTimeStrikeGuess
      -> ReaderT SqlBackend (NoLoggingT (ResourceT IO))
         [(Entity V1.BlockTimeStrikeGuess, Entity V1.BlockTimeStrike)]
    maybeFetchBlockTimeStrikeByGuess strikeFilter
        guessE@(Entity _guessId guess) = do
      maybe [] (\strikeE -> [(guessE, strikeE)])
        <$> selectFirst
          ( ( V1.BlockTimeStrikeId ==. V1.blockTimeStrikeGuessStrike guess)
          : strikeFilter
          )
          []
    fetchBlockTimeStrikeObservedByStrike
      :: [Filter V1.BlockTimeStrikeObserved]
      -> (Entity V1.BlockTimeStrikeGuess, Entity V1.BlockTimeStrike)
      -> ReaderT SqlBackend (NoLoggingT (ResourceT IO))
         [ ( Entity V1.BlockTimeStrikeGuess
           , Entity V1.BlockTimeStrike
           , Maybe (Entity V1.BlockTimeStrikeObserved)
           )
         ]
    fetchBlockTimeStrikeObservedByStrike observedStaticFilter
        (guessE, strikeE@(Entity strikeId _)) = do
      mObserved <- selectFirst
        ( (V1.BlockTimeStrikeObservedStrike ==. strikeId)
        : observedStaticFilter
        )
        []
      return [(guessE, strikeE, mObserved)]
    maybeFetchCalculatedGuessesCount
      :: ( Entity V1.BlockTimeStrikeGuess
         , Entity V1.BlockTimeStrike
         , Maybe (Entity V1.BlockTimeStrikeObserved)
         )
      -> ReaderT SqlBackend (NoLoggingT (ResourceT IO))
         [ ( Entity V1.BlockTimeStrikeGuess
           , Entity V1.BlockTimeStrike
           , Maybe (Entity V1.BlockTimeStrikeObserved)
           , Entity V1.CalculatedBlockTimeStrikeGuessesCount
           )
         ]
    maybeFetchCalculatedGuessesCount
        ( guessE
        , strikeE@(Entity strikeId _)
        , observedE
        ) = do
      maybe [] (\calculatedE -> [(guessE, strikeE, observedE, calculatedE)])
        <$> selectFirst
          [V1.CalculatedBlockTimeStrikeGuessesCountStrike ==. strikeId]
          []
    coerceFilterRequestBlockTimeStrike
      :: APIV1.BuildFilter V1.BlockTimeStrike a
      => APIV1.FilterRequest BlockSpanTimeStrikeGuess a
      -> APIV1.FilterRequest V1.BlockTimeStrike a
    coerceFilterRequestBlockTimeStrike = APIV1.FilterRequest
      . (\(f, _)-> (f, Proxy))
      . APIV1.unFilterRequest
    mfilter = fmap coerceFilterRequestBlockTimeStrike mfilterAPI
    coerceFilterRequestBlockTimeStrikeGuess
      :: APIV1.BuildFilter V1.BlockTimeStrikeGuess a
      => APIV1.FilterRequest BlockSpanTimeStrikeGuess a
      -> APIV1.FilterRequest V1.BlockTimeStrikeGuess a
    coerceFilterRequestBlockTimeStrikeGuess = APIV1.FilterRequest
      . (\(f, _)-> (f, Proxy))
      . APIV1.unFilterRequest
    mfilterGuess = fmap coerceFilterRequestBlockTimeStrikeGuess mfilterAPI
    coerceFilterRequestBlockTimeStrikeObserved
      :: APIV1.BuildFilter V1.BlockTimeStrikeObserved a
      => APIV1.FilterRequest BlockSpanTimeStrikeGuess a
      -> APIV1.FilterRequest V1.BlockTimeStrikeObserved a
    coerceFilterRequestBlockTimeStrikeObserved = APIV1.FilterRequest
      . (\(f, _)-> (f, Proxy))
      . APIV1.unFilterRequest
    mfilterObserved = fmap coerceFilterRequestBlockTimeStrikeObserved mfilterAPI
    observedStaticFilter = maybe
      []
      ( APIV1.buildFilter
      . APIV1.unFilterRequest
      . APIV1.coerceFilter
      )
      mfilterObserved
    strikeStaticPartFilter = maybe
      []
      ( APIV1.buildFilter
      . APIV1.unFilterRequest
      . APIV1.coerceFilter
      )
      mfilter
    guessStaticPartFilter = maybe
      []
      ( APIV1.buildFilter
      . APIV1.unFilterRequest
      . APIV1.coerceFilter
      )
      mfilterGuess
    sort = maybe
             Descend
             ( APIV1.sortOrder
             . APIV1.unFilterRequest
             )
             mfilter

