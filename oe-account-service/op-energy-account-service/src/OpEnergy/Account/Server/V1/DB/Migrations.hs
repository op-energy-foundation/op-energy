-- | this module defines tables for tracking account and blocktimestrike dbs' versions
-- the reason why they are separate is because we viewed them as a separate component, which
-- may or may not be splitted into services later
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls  #-}


module OpEnergy.Account.Server.V1.DB.Migrations
  where

import           Database.Persist.TH
import           Database.Persist.Sql

import           Data.OpEnergy.API.V1.Natural(Natural)
import           Data.OpEnergy.API.V1.Block(BlockHeight)
import           OpEnergy.BlockTimeStrike.Server.V1.Context(Context)



share [mkPersist sqlSettings, mkMigrate "migrateAccountDB"] [persistLowerCase|

-- | this table will contain only one record, which contain version of DB, which will be
-- updated at each custom migration
AccountDB
  version (Natural Int)
  deriving Eq Show

|]

data LatestObservedConfirmedTip
share [mkPersist sqlSettings, mkMigrate "migrateBlockTimeStrikeDB"] [persistLowerCase|

-- | this table will contain only one record, which contain version of DB, which will be
-- updated at each custom migration
BlockTimeStrikeDB
  version (Natural Int)
  latestConfirmedHeight (Context LatestObservedConfirmedTip BlockHeight) Maybe

|]
