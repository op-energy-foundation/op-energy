{-- | This module implements BlockTime strike service.
 -}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GADTs                      #-}
module OpEnergy.BlockTimeStrike.Server.V2.BlockSpanTimeStrikeService.GetStrikes
  ( getStrikes
  ) where

import           Servant ( err500)
import           Control.Monad.Trans.Reader ( asks)
import           Control.Monad.Logger( logError)
import           Control.Monad(forM)
import           Control.Monad.Trans
import           Control.Monad.Trans.Except( ExceptT (..))

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
import qualified OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeService
                 as V1
import qualified OpEnergy.Account.Server.V1.Config as Config
import           OpEnergy.Account.Server.V1.Class
                 ( AppM, State(..), runLogging, profile )
import qualified OpEnergy.BlockTimeStrike.Server.V2.BlockSpanTimeStrike
                 as BlockSpanTimeStrike

-- | this handler returns pageable result of the BlockSpanTimeStrikes for a
-- given spanSize, page and filter
getStrikes
  :: Maybe (APIV1.Positive Int)
  -> Maybe (APIV1.Natural Int)
  -> Maybe ( APIV1.FilterRequest
             APIV1.BlockTimeStrike
             APIV1.BlockTimeStrikeFilter
           )
  -> AppM (APIV1.PagingResult BlockSpanTimeStrike)
getStrikes mspanSize mpage mfilter =
    let name = "V2.getStrikes"
    in profile name $ eitherThrowJSON
      (\reason-> do
        callstack <- asks callStack
        runLogging $ $(logError) $ callstack <> ":" <> reason
        return (err500, reason)
      )
      $ runExceptPrefixT name $ do
  blockTimeStrikePage <- ExceptT $ V1.getBlockTimeStrikesPage mpage mfilter
  spanSize <- lift $ maybe
    (asks $ Config.configBlockSpanDefaultSize . config)
    pure
    mspanSize
  blockSpanTimeStrikes <- forM
        (APIV1.pagingResultResults blockTimeStrikePage)
        ( ExceptT
        . BlockSpanTimeStrike.apiBlockSpanTimeStrikeModelBlockTimeStrike spanSize
        . APIV1.blockTimeStrikeWithGuessesCountStrike
        )
  return $! APIV1.PagingResult
    { APIV1.pagingResultNextPage =
      APIV1.pagingResultNextPage blockTimeStrikePage
    , APIV1.pagingResultResults = blockSpanTimeStrikes
    }

