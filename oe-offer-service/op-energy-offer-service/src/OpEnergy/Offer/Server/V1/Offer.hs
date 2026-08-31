{-- | This module defines the Offer Persistent entity: one row per posted
 - maker offer, created by OpEnergy.Offer.Server.V2.PostOfferAPI.Post.
 -
 - 'personUUID' is a value FK into oe-account-service's own Person table --
 - not a real Postgres foreign key, since that table lives in a different
 - service's database. It is resolved once per request, from the
 - AccountToken header, via OpEnergy.Offer.Server.V1.AccountClient (which
 - calls oe-account-service's GET /api/v2/account/whoami), never read back
 - out of this service's own DB as an authority on identity.
 -
 - 'creatorDisplayName' is a *snapshot* of the creator's display name taken
 - at post time, not a live join (there is no local Person table left to
 - join against, post-split) and not re-resolved cross-service on every
 - read either -- a public, unauthenticated GET .../offer/list that fanned
 - out one cross-service call per row shown would be one dependency and one
 - latency source too many for a read this cheap. This means it can go
 - stale if the creator later renames (oe-account-service has no
 - displayname-rename call yet on this branch, so it can't happen today,
 - but the field is named to make clear it would not track a future one).
 - See docs/plans/post-offer-api.md's "known gaps" for the tradeoff this
 - accepts, and the future-work note (a public resolve-by-uuid endpoint on
 - oe-account-service) if it needs to stop being a snapshot.
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
import           Data.OpEnergy.Offer.API.V1.OfferStatus(OfferStatus)
import qualified Data.OpEnergy.Offer.API.V2.OffersAPI as API
import           Data.Text.Show(tshow)

share [mkPersist sqlSettings, mkMigrate "migrateOffer"] [persistLowerCase|

-- | one row per posted maker offer
Offer
  personUUID (AccountAPI.UUID AccountAPI.Person)
  -- ^ who posted this offer -- see module header: a value FK, resolved
  -- cross-service, not a real Postgres FK
  creatorDisplayName AccountAPI.DisplayName
  -- ^ snapshot at post time -- see module header
  targetBlock (Natural Int)
  -- ^ the block height this offer's "before" trade is predicted against
  validTillBlock (Natural Int)
  -- ^ block height after which this offer is no longer acceptable
  -- (Phase 1: informational only, not enforced by a sweep job)
  makerStakeSats Word64
  -- ^ sats staked per offer, deducted from the creator's balance (held by
  -- oe-account-service) before this row's insert -- see
  -- OpEnergy.Offer.Server.V2.PostOfferAPI.Post
  status OfferStatus
  -- ^ always "open" at insert (Phase 1 has no matching engine to ever move
  -- it past that except by expiring/cancelling it)
  expiresAt UTCTime Maybe
  -- ^ optional wall-clock deadline. Nothing for every offer posted so far --
  -- there's no reliable way to convert this offer's own block-height-based
  -- validTillBlock into a real timestamp without assuming a block rate,
  -- which is exactly the thing being predicted. validTillBlock remains the
  -- actual source of truth for "is this still postable"; this column is
  -- reserved for whenever a real rate feed can back-fill it honestly.
  refundedAt UTCTime Maybe
  -- ^ set (alongside status) the moment this offer's stake is refunded --
  -- by OpEnergy.Offer.Server.V2.Expiry or the cancel endpoint. Nothing
  -- while "open".
  created UTCTime
  -- ^ time this offer was posted
  deriving Eq Show
|]

-- | Model -> API glue (per this project's API/Model separation convention --
-- see e.g. OpEnergy.Account.Server.V1.Person's apiModelPerson): an Offer row
-- plus its own (already-known) stringified key -> the OfferInfo DTO that
-- crosses the API boundary. Shared by every handler that renders an Offer
-- (Post, GetMine, GetList, GetById, Cancel) so the mapping is written once.
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

-- | same as 'offerInfoFrom', for an 'Entity Offer' fetched by key (the
-- common case for every read path) -- derives the id text from the key
-- itself rather than requiring the caller to have it separately.
offerInfoFromEntity :: Entity Offer -> API.OfferInfo
offerInfoFromEntity (Entity key offerVal) = offerInfoFrom (tshow (fromSqlKey key)) offerVal
