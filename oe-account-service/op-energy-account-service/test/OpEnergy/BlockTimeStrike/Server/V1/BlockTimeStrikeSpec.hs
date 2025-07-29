module OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrikeSpec
  ( spec
  ) where

import qualified Data.Aeson as Aeson
import qualified Database.Persist as Persist
import           Test.QuickCheck
import           Test.Hspec

import           Data.OpEnergy.Account.API.V1.BlockTimeStrike

spec :: Spec
spec = do
  describe "BlockTimeStrike serialization" $ do
    it "from/to JSON should be the same" $
      property $ \(x::BlockTimeStrike) ->
        case Aeson.eitherDecode (Aeson.encode x) of
          Right some
            | some == x -> True
          _ -> False

    it "from/to Persistent should be the same" $
      property $ \(x::BlockTimeStrike) ->
        case Persist.fromPersistValue (Persist.toPersistValue x) of
          Right some
            | some == x -> True
          _ -> False

    it "from/to JSON should be the same" $
      property $ \(x::BlockTimeStrikeObservedPublic) ->
        case Aeson.eitherDecode (Aeson.encode x) of
          Right some
            | some == x -> True
          _ -> False

    it "from/to Persistent should be the same" $
      property $ \(x::BlockTimeStrikeObserved) ->
        case Persist.fromPersistValue (Persist.toPersistValue x) of
          Right some
            | some == x -> True
          _ -> False
