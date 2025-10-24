{-# LANGUAGE EmptyDataDecls          #-}
module OpEnergy.BlockTimeStrike.Server.V2.BlockSpanTimeStrikeJudgement
  ( judgeStrike
  , BlockObserved
  , BlockMediantimeReachedBlockNotObserved
  , JudgementBlock
  , eitherBlockOrMediantimeObservedStrike
  ) where

import           Database.Persist

import           Data.OpEnergy.API.V1.Block
import           OpEnergy.BlockTimeStrike.Server.V1.Context (Context, unContext)
import qualified OpEnergy.BlockTimeStrike.Server.V1.Context as Context
import           OpEnergy.BlockTimeStrike.Server.V1.SlowFast

import qualified OpEnergy.BlockTimeStrike.Server.V2.DBModel as DB


data BlockObserved
data BlockMediantimeReachedBlockNotObserved
data JudgementBlock

eitherBlockOrMediantimeObservedStrike
  :: Entity DB.BlockSpanTimeStrike
  -> BlockHeader
  -> Either (Context BlockObserved (Entity DB.BlockSpanTimeStrike))
            (Context BlockMediantimeReachedBlockNotObserved
              (Entity DB.BlockSpanTimeStrike)
            )
eitherBlockOrMediantimeObservedStrike strikeE@(Entity _ strike) confirmedTip =
  if isStrikeBlockConfirmed
    then Left $ Context.believeme strikeE
    else Right $ Context.believeme strikeE
  where
  isStrikeBlockConfirmed =
    DB.blockSpanTimeStrikeBlock strike <= blockHeaderHeight confirmedTip

judgeStrike
  :: Either (Context BlockObserved (Entity DB.BlockSpanTimeStrike))
            (Context BlockMediantimeReachedBlockNotObserved
              (Entity DB.BlockSpanTimeStrike)
            )
  -> Context JudgementBlock BlockHeader
  -> SlowFast
judgeStrike estrikeC judgementBlockC =
  if isBlockDiscoveredFasterThanStrikeMediantime
    then Fast
    else Slow
  where
    judgementBlock = unContext judgementBlockC
    isBlockDiscoveredFasterThanStrikeMediantime =
      case estrikeC of
        Right _ -> False
        Left blockObservedStrike ->
          let
              Entity _ strike = unContext blockObservedStrike
          in fromIntegral (blockHeaderMediantime judgementBlock)
               < DB.blockSpanTimeStrikeMediantime strike

