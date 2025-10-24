{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DerivingStrategies         #-}

module OpEnergy.BlockTimeStrike.Server.V2.DBModel
  ( module OpEnergy.BlockTimeStrike.Server.V1.DB.Migrations.BlockTimeStrikeToBlockSpanTimeStrike.ModelAfter
  )
  where

import           Database.Persist

import           OpEnergy.BlockTimeStrike.Server.V1.DB.Migrations.BlockTimeStrikeToBlockSpanTimeStrike.ModelAfter

