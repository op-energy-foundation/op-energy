-- | this module defines the table for tracking this service's own DB
-- version, same purpose/shape as oe-account-service's
-- OpEnergy.Account.Server.V1.DB.Migrations.AccountDB
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
module OpEnergy.Offer.Server.V1.DB.Migrations
  where

import           Database.Persist.TH

import           Data.OpEnergy.API.V1.Natural(Natural)

share [mkPersist sqlSettings, mkMigrate "migrateOfferDB"] [persistLowerCase|

-- | this table will contain only one record, which contains the version of
-- this service's DB, updated at each custom migration
OfferDB
  version (Natural Int)
  deriving Eq Show

|]
