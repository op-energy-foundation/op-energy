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



share [mkPersist sqlSettings, mkMigrate "migrateBlockTimeStrikeDB"] [persistLowerCase|

-- | this table will contain only one record, which contain version of DB, which will be
-- updated at each custom migration
BlockTimeStrikeDB
  version (Natural Int)
  deriving Eq Show

|]
