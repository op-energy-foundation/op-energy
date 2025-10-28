{-- | This module implements BlockTime strike service.
 -}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DataKinds                  #-}
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
module OpEnergy.BlockTimeStrike.Server.V1.BlockTimeStrike
  where

import           Data.Time.Clock.POSIX(POSIXTime)
import           GHC.Generics
import qualified Data.List as List

import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Pagination
import           Database.Persist.TH
import           Data.Proxy


import           Data.OpEnergy.API.V1.Block
import           Data.OpEnergy.Account.API.V1.BlockTimeStrikeGuess
import           Data.OpEnergy.Account.API.V1.FilterRequest
import qualified Data.OpEnergy.Account.API.V1.BlockTimeStrike as API
import qualified Data.OpEnergy.Account.API.V1.BlockTimeStrikePublic as API
import           OpEnergy.BlockTimeStrike.Server.V1.SlowFast(SlowFast)
import qualified OpEnergy.BlockTimeStrike.Server.V1.SlowFast as SlowFast

share [mkPersist sqlSettings, mkMigrate "migrateBlockTimeStrike"] [persistLowerCase|
BlockTimeStrike
  -- data
  block BlockHeight
  strikeMediantime POSIXTime
  -- metadata
  creationTime POSIXTime
  -- constraints
  UniqueBlockTimeStrikeBlockStrikeMediantime block strikeMediantime -- for now it is forbidden to have multiple strikes of the same (block,strikeMediantime) values
  deriving Eq Show Generic

BlockTimeStrikeObserved
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
  strike BlockTimeStrikeId
  -- constraints
  UniqueBlocktimeStrikeObservationStrike strike -- unique per strike
  deriving Eq Show Generic

|]

instance BuildFilter BlockTimeStrike BlockTimeStrikeGuessResultPublicFilter where
  sortOrder (filter, _) = maybe Descend id (blockTimeStrikeGuessResultPublicFilterSort filter)
  buildFilter ( BlockTimeStrikeGuessResultPublicFilter
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
    [ maybe [] (\v -> [ BlockTimeStrikeBlock >=. v]) mStrikeBlockHeightGTE
    , maybe [] (\v -> [ BlockTimeStrikeBlock <=. v]) mStrikeBlockHeightLTE
    , maybe [] (\v -> [ BlockTimeStrikeBlock ==. v]) mStrikeBlockHeightEQ
    , maybe [] (\v -> [ BlockTimeStrikeBlock !=. v]) mStrikeBlockHeightNEQ
    , maybe [] (\v -> [ BlockTimeStrikeStrikeMediantime >=. v]) mStrikeMediantimeGTE
    , maybe [] (\v -> [ BlockTimeStrikeStrikeMediantime <=. v]) mStrikeMediantimeLTE
    , maybe [] (\v -> [ BlockTimeStrikeStrikeMediantime ==. v]) mStrikeMediantimeEQ
    , maybe [] (\v -> [ BlockTimeStrikeStrikeMediantime !=. v]) mStrikeMediantimeNEQ
    ]
coerceFilterRequestBlockTimeStrike
  :: BuildFilter BlockTimeStrike a
  => FilterRequest API.BlockTimeStrikePublic a
  -> FilterRequest BlockTimeStrike a
coerceFilterRequestBlockTimeStrike = FilterRequest
  . (\(f, _)-> (f, Proxy))
  . unFilterRequest

instance BuildFilter BlockTimeStrikeObserved API.BlockTimeStrikeFilter where
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
    [ maybe [] (\v-> [BlockTimeStrikeObservedJudgementBlockHash ==. v]) mobservedBlockHashEQ
    , maybe [] (\v-> [BlockTimeStrikeObservedJudgementBlockHash !=. v]) mobservedBlockHashNEQ
    , maybe [] (\v-> [ BlockTimeStrikeObservedIsFast
                       ==. SlowFast.modelApi v
                     ]) mobservedResultEQ
    , maybe [] (\v-> [ BlockTimeStrikeObservedIsFast
                       !=. SlowFast.modelApi v
                     ]) mobservedResultNEQ
    ]

instance BuildFilter BlockTimeStrikeObserved BlockTimeStrikeGuessResultPublicFilter where
  sortOrder (filter, _) = maybe Descend id (blockTimeStrikeGuessResultPublicFilterSort filter)
  buildFilter ( BlockTimeStrikeGuessResultPublicFilter
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
    [ maybe [] (\v -> [ BlockTimeStrikeObservedIsFast
                        ==. SlowFast.modelApi v
                      ]) mObservedResultEQ
    , maybe [] (\v -> [ BlockTimeStrikeObservedIsFast
                        !=. SlowFast.modelApi v
                      ]) mObservedResultNEQ
    ]

instance BuildFilter BlockTimeStrike API.BlockTimeStrikeFilter where
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
    [ maybe [] (\v-> [BlockTimeStrikeStrikeMediantime >=. v]) mstrikeMediantimeGTE
    , maybe [] (\v-> [BlockTimeStrikeStrikeMediantime <=. v]) mstrikeMediantimeLTE
    , maybe [] (\v-> [BlockTimeStrikeStrikeMediantime ==. v]) mstrikeMediantimeEQ
    , maybe [] (\v-> [BlockTimeStrikeStrikeMediantime !=. v]) mstrikeMediantimeNEQ
    , maybe [] (\v-> [BlockTimeStrikeBlock >=. v]) mstrikeBlockHeightGTE
    , maybe [] (\v-> [BlockTimeStrikeBlock <=. v]) mstrikeBlockHeightLTE
    , maybe [] (\v-> [BlockTimeStrikeBlock ==. v]) mstrikeBlockHeightEQ
    , maybe [] (\v-> [BlockTimeStrikeBlock !=. v]) mstrikeBlockHeightNEQ
    ]


apiModelBlockTimeStrike
  :: BlockTimeStrike
  -> API.BlockTimeStrike
apiModelBlockTimeStrike v = API.BlockTimeStrike
  { API.blockTimeStrikeBlock = blockTimeStrikeBlock v
  , API.blockTimeStrikeStrikeMediantime = blockTimeStrikeStrikeMediantime v
  , API.blockTimeStrikeCreationTime = blockTimeStrikeCreationTime v
  }

apiModelBlockTimeStrikePublic
  :: BlockTimeStrike
  -> Maybe BlockTimeStrikeObserved
  -> API.BlockTimeStrikePublic
apiModelBlockTimeStrikePublic v mObserved = API.BlockTimeStrikePublic
  { API.blockTimeStrikePublicBlock = blockTimeStrikeBlock v
  , API.blockTimeStrikePublicStrikeMediantime = blockTimeStrikeStrikeMediantime v
  , API.blockTimeStrikePublicCreationTime = blockTimeStrikeCreationTime v
  , API.blockTimeStrikePublicObservedResult =
    fmap ( SlowFast.apiModel . blockTimeStrikeObservedIsFast) mObserved
  , API.blockTimeStrikePublicObservedBlockMediantime =
    fmap blockTimeStrikeObservedJudgementBlockMediantime mObserved
  , API.blockTimeStrikePublicObservedBlockHash =
    fmap blockTimeStrikeObservedJudgementBlockHash mObserved
  , API.blockTimeStrikePublicObservedBlockHeight =
    fmap blockTimeStrikeObservedJudgementBlockHeight mObserved
  }

modelApiBlockTimeStrike
  :: API.BlockTimeStrike
  -> BlockTimeStrike
modelApiBlockTimeStrike v = BlockTimeStrike
  { blockTimeStrikeBlock = API.blockTimeStrikeBlock v
  , blockTimeStrikeStrikeMediantime = API.blockTimeStrikeStrikeMediantime v
  , blockTimeStrikeCreationTime = API.blockTimeStrikeCreationTime v
  }


