{-# LANGUAGE EmptyDataDecls          #-}
module OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeJudgement
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
import           Data.OpEnergy.Account.API.V1.BlockTimeStrike


data BlockObserved
data BlockMediantimeReachedBlockNotObserved
data JudgementBlock

eitherBlockOrMediantimeObservedStrike
  :: Entity BlockTimeStrike
  -> BlockHeader
  -> ( Either (Context BlockObserved (Entity BlockTimeStrike))
              (Context BlockMediantimeReachedBlockNotObserved (Entity BlockTimeStrike))
     )
eitherBlockOrMediantimeObservedStrike strikeE@(Entity _ strike) confirmedTip =
  if isStrikeBlockConfirmed
    then Left $ Context.believeme strikeE
    else Right $ Context.believeme strikeE
  where
  isStrikeBlockConfirmed =
    blockTimeStrikeBlock strike <= blockHeaderHeight confirmedTip

judgeStrike
  :: ( Either (Context BlockObserved (Entity BlockTimeStrike))
              (Context BlockMediantimeReachedBlockNotObserved (Entity BlockTimeStrike))
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
          in fromIntegral (blockHeaderMediantime judgementBlock) < blockTimeStrikeStrikeMediantime strike

