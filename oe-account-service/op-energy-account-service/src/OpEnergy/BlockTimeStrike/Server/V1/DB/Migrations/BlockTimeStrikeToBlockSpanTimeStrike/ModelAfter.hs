
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE DerivingStrategies         #-}

module OpEnergy.BlockTimeStrike.Server.V1.DB.Migrations.BlockTimeStrikeToBlockSpanTimeStrike.ModelAfter
  where

import           GHC.Generics
import qualified Data.Text as Text
import           Data.Maybe

import           Database.Persist.TH
import           Database.Persist
import           Database.Persist.Postgresql

import           Data.Time.Clock.POSIX(POSIXTime)
import qualified Data.List as List

import           Database.Persist.Pagination(SortOrder(..))
import           Data.Proxy
import qualified Data.Serialize             as S


import           Data.OpEnergy.API.V1.Natural
import           Data.OpEnergy.API.V1.Positive
import           Data.OpEnergy.API.V1.Block
import qualified Data.OpEnergy.Account.API.V1.FilterRequest as API
import qualified Data.OpEnergy.Account.API.V1.BlockTimeStrike as API
import qualified Data.OpEnergy.Account.API.V1.BlockTimeStrikeGuess as API
import           OpEnergy.Account.Server.V1.Person
import           OpEnergy.BlockTimeStrike.Server.V1.SlowFast(SlowFast)
import qualified OpEnergy.BlockTimeStrike.Server.V1.SlowFast as SlowFast


-- | here goes migration-local representations of the tables.
share [mkPersist sqlSettings, mkMigrate "migrateModels"] [persistLowerCase|
BlockSpanTimeStrike
  -- data
  block BlockHeight
  spanSize (Positive Int)
  mediantime POSIXTime
  -- metadata
  creationTime POSIXTime
  -- constraints
  UniqueBlockSpanTimeStrikeBlockStrikeMediantime block mediantime -- for now it is forbidden to have multiple strikes of the same (block,strikeMediantime) values
  deriving Eq Show Generic

BlockSpanTimeStrikeObserved
  -- data
  -- those fields are being kept as a sanity check in case of block chain
  -- reorganization as a last prove of the outcome. Though, we use confirmation
  -- algorithm, which goal is to reduce a possibility of hitting this case
  judgementBlockMediantime POSIXTime -- mediantime of the judgement block.
  judgementBlockHash BlockHash -- hash of the judgement block.
  judgementBlockHeight BlockHeight -- height of the judgement block.
  isFast SlowFast
  -- metadata
  creationTime POSIXTime
  -- reflinks
  strike BlockSpanTimeStrikeId
  -- constraints
  UniqueBlocktimeStrikeObservationStrike strike -- unique per strike
  deriving Eq Show Generic

BlockSpanTimeStrikeGuess
  -- data
  isFast SlowFast
  -- metadata
  creationTime POSIXTime
  -- reflinks
  strike BlockSpanTimeStrikeId
  person PersonId
  -- constraints
  UniqueBlockSpanTimeStrikeGuessPersonStrike person strike -- only 1 guess per strike is allowed for person
  deriving Eq Show Generic

-- this table's goal is to contain precalculated guesses count for a given strike. The reason
-- it exists is to eliminate a need of walking through the db in order to return a result
-- sorted by guesses count
CalculatedBlockSpanTimeStrikeGuessesCount
  strike BlockSpanTimeStrikeId
  guessesCount (Natural Int)
  UniqueCalculatedBlockSpanTimeStrikeGuessesCountStrike strike -- allow only one record per strike
  deriving Eq Show Generic

|]

instance PersistField (Positive Int) where
  toPersistValue (Positive s) = toPersistValue s
  fromPersistValue (PersistInt64 s) = Right $! verifyPositiveInt (fromIntegral s) -- TODO
  fromPersistValue _ = Left $ "InputVerification.hs fromPersistValue Natural, expected Text"
instance PersistFieldSql (Positive Int) where
  sqlType _ = SqlInt64

instance PersistField (Positive Integer) where
  toPersistValue (Positive i) = toPersistValue (S.encode i) -- store as bytestring
  fromPersistValue (PersistByteString s) =
    case S.decode s of
      Left some -> Left (Text.pack some)
      Right ret -> everifyPositive ret
  fromPersistValue _ = Left $ "InputVerification.hs fromPersistValue Natural, expected Text"
instance PersistFieldSql (Positive Integer) where
  sqlType _ = SqlBlob


instance API.BuildFilter BlockSpanTimeStrike API.BlockTimeStrikeGuessResultPublicFilter where
  sortOrder (filter, _) = fromMaybe
    Descend
    (API.blockTimeStrikeGuessResultPublicFilterSort filter)
  buildFilter ( API.BlockTimeStrikeGuessResultPublicFilter
                -- person
                _
                _
                -- guess
                _
                _
                -- observedResult
                _
                _
                -- strike block height
                mStrikeBlockHeightGTE
                mStrikeBlockHeightLTE
                mStrikeBlockHeightEQ
                mStrikeBlockHeightNEQ
                -- strike strikeMediantime
                mStrikeMediantimeGTE
                mStrikeMediantimeLTE
                mStrikeMediantimeEQ
                mStrikeMediantimeNEQ
                -- sort
                _
                _
                _ -- lines per page
              , _
              ) = List.concat
        -- strike block height
    [ maybe [] (\v -> [ BlockSpanTimeStrikeBlock >=. v]) mStrikeBlockHeightGTE
    , maybe [] (\v -> [ BlockSpanTimeStrikeBlock <=. v]) mStrikeBlockHeightLTE
    , maybe [] (\v -> [ BlockSpanTimeStrikeBlock ==. v]) mStrikeBlockHeightEQ
    , maybe [] (\v -> [ BlockSpanTimeStrikeBlock !=. v]) mStrikeBlockHeightNEQ
    , maybe [] (\v -> [ BlockSpanTimeStrikeMediantime >=. v])
      mStrikeMediantimeGTE
    , maybe [] (\v -> [ BlockSpanTimeStrikeMediantime <=. v])
      mStrikeMediantimeLTE
    , maybe [] (\v -> [ BlockSpanTimeStrikeMediantime ==. v])
      mStrikeMediantimeEQ
    , maybe [] (\v -> [ BlockSpanTimeStrikeMediantime !=. v])
      mStrikeMediantimeNEQ
    ]
coerceFilterRequestBlockSpanTimeStrike
  :: API.BuildFilter BlockSpanTimeStrike a
  => API.FilterRequest API.BlockTimeStrike a
  -> API.FilterRequest BlockSpanTimeStrike a
coerceFilterRequestBlockSpanTimeStrike = API.FilterRequest
  . (\(f, _)-> (f, Proxy))
  . API.unFilterRequest

instance API.BuildFilter BlockSpanTimeStrikeObserved API.BlockTimeStrikeFilter where
  sortOrder (filter, _) = maybe
                            Descend
                            API.sortOrderFromStrikeSortOrder
                            (API.blockTimeStrikeFilterSort filter)
  buildFilter ( API.BlockTimeStrikeFilter
                _
                _
                _
                _
                _
                _
                _
                _
                mobservedBlockHashEQ
                mobservedBlockHashNEQ
                mobservedResultEQ
                mobservedResultNEQ
                _ -- sort
                _ -- class
                _ -- linesPerPage
              , _
              ) = List.concat
    [ maybe [] (\v-> [BlockSpanTimeStrikeObservedJudgementBlockHash ==. v])
      mobservedBlockHashEQ
    , maybe [] (\v-> [BlockSpanTimeStrikeObservedJudgementBlockHash !=. v])
      mobservedBlockHashNEQ
    , maybe [] (\v-> [ BlockSpanTimeStrikeObservedIsFast
                       ==. SlowFast.modelApiSlowFast v
                     ]) mobservedResultEQ
    , maybe [] (\v-> [ BlockSpanTimeStrikeObservedIsFast
                       !=. SlowFast.modelApiSlowFast v
                     ]) mobservedResultNEQ
    ]

instance API.BuildFilter BlockSpanTimeStrikeObserved API.BlockTimeStrikeGuessResultPublicFilter where
  sortOrder (filter, _) = fromMaybe
    Descend
    (API.blockTimeStrikeGuessResultPublicFilterSort filter)
  buildFilter ( API.BlockTimeStrikeGuessResultPublicFilter
                -- person
                _
                _
                -- guess
                _
                _
                -- observedResult
                mObservedResultEQ
                mObservedResultNEQ
                -- strike block height
                _
                _
                _
                _
                -- strike strikeMediantime
                _
                _
                _
                _
                -- sort
                _
                _
                _ -- lines per page
              , _
              ) = List.concat
        -- strike observed result
    [ maybe [] (\v -> [ BlockSpanTimeStrikeObservedIsFast
                        ==. SlowFast.modelApiSlowFast v
                      ]) mObservedResultEQ
    , maybe [] (\v -> [ BlockSpanTimeStrikeObservedIsFast
                        !=. SlowFast.modelApiSlowFast v
                      ]) mObservedResultNEQ
    ]

instance API.BuildFilter BlockSpanTimeStrike API.BlockTimeStrikeFilter where
  sortOrder (filter, _) = maybe
                            Descend
                            API.sortOrderFromStrikeSortOrder
                            (API.blockTimeStrikeFilterSort filter)
  buildFilter ( API.BlockTimeStrikeFilter
                mstrikeMediantimeGTE
                mstrikeMediantimeLTE
                mstrikeMediantimeEQ
                mstrikeMediantimeNEQ
                mstrikeBlockHeightGTE
                mstrikeBlockHeightLTE
                mstrikeBlockHeightEQ
                mstrikeBlockHeightNEQ
                _  -- mobservedBlockHashEQ
                _  -- mobservedBlockHashNEQ
                _  -- mobservedResultEQ
                _  -- mobservedResultNEQ
                _ -- sort
                _ -- class
                _ -- linesPerPage
              , _
              ) = List.concat
    [ maybe [] (\v-> [BlockSpanTimeStrikeMediantime >=. v])
      mstrikeMediantimeGTE
    , maybe [] (\v-> [BlockSpanTimeStrikeMediantime <=. v])
      mstrikeMediantimeLTE
    , maybe [] (\v-> [BlockSpanTimeStrikeMediantime ==. v])
      mstrikeMediantimeEQ
    , maybe [] (\v-> [BlockSpanTimeStrikeMediantime !=. v])
      mstrikeMediantimeNEQ
    , maybe [] (\v-> [BlockSpanTimeStrikeBlock >=. v]) mstrikeBlockHeightGTE
    , maybe [] (\v-> [BlockSpanTimeStrikeBlock <=. v]) mstrikeBlockHeightLTE
    , maybe [] (\v-> [BlockSpanTimeStrikeBlock ==. v]) mstrikeBlockHeightEQ
    , maybe [] (\v-> [BlockSpanTimeStrikeBlock !=. v]) mstrikeBlockHeightNEQ
    ]


apiBlockTimeStrikeModelBlockSpanTimeStrike
  :: BlockSpanTimeStrike
  -> API.BlockTimeStrike
apiBlockTimeStrikeModelBlockSpanTimeStrike v = API.BlockTimeStrike
  { API.blockTimeStrikeBlock = blockSpanTimeStrikeBlock v
  , API.blockTimeStrikeStrikeMediantime = blockSpanTimeStrikeMediantime v
  , API.blockTimeStrikeCreationTime = blockSpanTimeStrikeCreationTime v
  }

-- modelBlockSpanTimeApiBlockTimeStrike
--   :: API.BlockTimeStrike
--   -> BlockSpanTimeStrike
-- modelBlockSpanTimeApiBlockTimeStrike v = BlockSpanTimeStrike
--   { blockSpanTimeStrikeBlock = API.blockTimeStrikeBlock v
--   , blockSpanTimeStrikeMediantime = API.blockTimeStrikeStrikeMediantime v
--   , blockSpanTimeStrikeCreationTime = API.blockTimeStrikeCreationTime v
--   }


instance API.BuildFilter BlockSpanTimeStrikeGuess API.BlockTimeStrikeGuessResultPublicFilter where
  sortOrder (filter, _) = fromMaybe Descend (API.blockTimeStrikeGuessResultPublicFilterSort filter)
  buildFilter ( API.BlockTimeStrikeGuessResultPublicFilter
                _
                _
                -- guess
                mGuessEQ
                mGuessNEQ
                -- observedResult
                _
                _
                -- strike block height
                _
                _
                _
                _
                -- strike strikeMediantime
                _
                _
                _
                _
                -- sort
                _
                _
                _ -- lines per page
              , _
              ) = List.concat
    [ maybe [] (\v-> [BlockSpanTimeStrikeGuessIsFast ==. SlowFast.modelApiSlowFast v]) mGuessEQ
    , maybe [] (\v-> [BlockSpanTimeStrikeGuessIsFast !=. SlowFast.modelApiSlowFast v]) mGuessNEQ
    ]

coerceFilterRequestBlockTimeStrikeGuess
  :: API.BuildFilter BlockSpanTimeStrikeGuess a
  => API.FilterRequest API.BlockTimeStrikeGuess a
  -> API.FilterRequest BlockSpanTimeStrikeGuess a
coerceFilterRequestBlockTimeStrikeGuess = API.FilterRequest
  . (\(f, _)-> (f, Proxy))
  . API.unFilterRequest

