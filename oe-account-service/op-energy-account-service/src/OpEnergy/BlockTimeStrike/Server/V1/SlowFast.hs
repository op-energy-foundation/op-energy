{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}
module OpEnergy.BlockTimeStrike.Server.V1.SlowFast
  ( SlowFast(..)
  , verifySlowFastEither
  , apiModelSlowFast
  , modelApiSlowFast
  ) where

import           Data.Text(Text)
import           GHC.Generics

import           Database.Persist
import           Database.Persist.Sql

import qualified Data.OpEnergy.Account.API.V1.SlowFast as API


data SlowFast
  = Slow
  | Fast
  deriving (Eq, Enum, Show, Bounded, Ord, Generic)

instance PersistField SlowFast where
  toPersistValue Slow = toPersistValue False
  toPersistValue Fast = toPersistValue True
  fromPersistValue (PersistBool False) = Prelude.Right Slow
  fromPersistValue (PersistBool True) = Prelude.Right Fast
  fromPersistValue _ = Left "fromPersistValue SlowFastGuess, unsupported value"
instance PersistFieldSql SlowFast where
  sqlType _ = SqlBool

verifySlowFastEither :: Text-> Either Text SlowFast
verifySlowFastEither "slow" = Right Slow
verifySlowFastEither "fast" = Right Fast
verifySlowFastEither v = Left ("verifySlowFast: wrong value: " <> v)

apiModelSlowFast
  :: SlowFast
  -> API.SlowFast
apiModelSlowFast v = case v of
  Slow -> API.Slow
  Fast -> API.Fast

modelApiSlowFast
  :: API.SlowFast
  -> SlowFast
modelApiSlowFast v = case v of
  API.Slow -> Slow
  API.Fast -> Fast
