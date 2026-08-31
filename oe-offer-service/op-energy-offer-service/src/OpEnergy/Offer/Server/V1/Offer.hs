{-- | This module defines the Offer Persistent entity.
 -
 - PersistField/PersistFieldSql instances for OfferStatus are placed here
 - (service layer) per the API-vs-Model separation convention, not in the
 - API module.
 -}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE RecordWildCards            #-}
module OpEnergy.Offer.Server.V1.Offer
  where

import           Data.Text(Text)
import           Data.Word(Word64)
import           Data.Time.Clock(UTCTime)

import           Database.Persist
import           Database.Persist.TH
import           Database.Persist.Sql

import           Data.OpEnergy.API.V1.Natural(Natural)
import qualified Data.OpEnergy.Account.API.V1.Account as AccountAPI
import qualified Data.OpEnergy.Account.API.V1.UUID as AccountAPI
import           Data.OpEnergy.Offer.API.V1.OfferStatus
                 ( OfferStatus, unOfferStatus, everifyOfferStatus
                 )
import qualified Data.OpEnergy.Offer.API.V2.OffersAPI as API
import           Data.Text.Show(tshow)

-- PersistField instances for OfferStatus -- placed here (service layer)
-- per the API-vs-Model separation convention, not in the API module.
instance PersistField OfferStatus where
  toPersistValue s = toPersistValue (unOfferStatus s)
  fromPersistValue (PersistText t) = either (\e -> Left ("OfferStatus fromPersistValue: " <> e)) Right
    $! everifyOfferStatus t
  fromPersistValue _ = Left "OfferStatus fromPersistValue, expected Text"
instance PersistFieldSql OfferStatus where
  sqlType _ = SqlString

share [mkPersist sqlSettings, mkMigrate "migrateOffer"] [persistLowerCase|

-- | one row per posted maker offer
Offer
  personUUID (AccountAPI.UUID AccountAPI.Person)
  creatorDisplayName AccountAPI.DisplayName
  targetBlock (Natural Int)
  validTillBlock (Natural Int)
  makerStakeSats Word64
  status OfferStatus
  expiresAt UTCTime Maybe
  refundedAt UTCTime Maybe
  created UTCTime
  deriving Eq Show
|]

-- | Model -> API glue
offerInfoFrom :: Text -> Offer -> API.OfferInfo
offerInfoFrom idText Offer{..} = API.OfferInfo
  { API.offerId = idText
  , API.creatorDisplayName = offerCreatorDisplayName
  , API.targetBlock = offerTargetBlock
  , API.validTillBlock = offerValidTillBlock
  , API.makerStakeSats = offerMakerStakeSats
  , API.status = offerStatus
  , API.expiresAt = offerExpiresAt
  , API.refundedAt = offerRefundedAt
  , API.created = offerCreated
  }

-- | same as 'offerInfoFrom', for an 'Entity Offer'
offerInfoFromEntity :: Entity Offer -> API.OfferInfo
offerInfoFromEntity (Entity key offerVal) = offerInfoFrom (tshow (fromSqlKey key)) offerVal
