module OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeJudgementSpec
  ( spec
  ) where

import           Database.Persist
import           Data.OpEnergy.API.V1.Block
import           Data.OpEnergy.Account.API.V1.BlockTimeStrike
import           OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeJudgement

import           Test.Hspec
import           Test.QuickCheck


spec :: Spec
spec = do
  describe "eitherBlockOrMediantimeObservedStrike test" $ do
    it "eitherBlockOrMediantimeObservedStrike" $ do
      property $ forAll blockHeaderAfterStrike $ \((strike, bh)::(BlockTimeStrike, BlockHeader)) ->
        let
          isConfirmedTipHeightMatchesStrikeHeight =
            blockHeaderHeight bh == blockTimeStrikeBlock strike
          isConfirmedTipMediantimeLessThanStrikeMediantime =
            ( blockHeaderHeight bh < blockTimeStrikeBlock strike
            && fromIntegral (blockHeaderMediantime bh) <= (blockTimeStrikeStrikeMediantime strike)
            )
        in
        case eitherBlockOrMediantimeObservedStrike (Entity undefined strike) bh of
          Left _
            | isConfirmedTipHeightMatchesStrikeHeight -> True
          Right _
            | isConfirmedTipMediantimeLessThanStrikeMediantime -> True
          _ -> False
  where
    blockHeaderAfterStrike = do
      strike <- arbitrary
      bh <- arbitrary
        `suchThat` (\x ->
          let
            isConfirmedTipHeightMatchesStrikeHeight = blockHeaderHeight x == blockTimeStrikeBlock strike
            isConfirmedTipMediantimeLessThanStrikeMediantime =
              ( blockHeaderHeight x < blockTimeStrikeBlock strike
              && fromIntegral (blockHeaderMediantime x) <= (blockTimeStrikeStrikeMediantime strike)
              )
          in
            isConfirmedTipHeightMatchesStrikeHeight
          || isConfirmedTipMediantimeLessThanStrikeMediantime
                   )
      return (strike, bh)

