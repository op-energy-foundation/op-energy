{-- | This module defines the Offer and Contract Persistent entities.
 -
 - PersistField/PersistFieldSql instances for OfferStatus, OfferSide and
 - ContractStatus are placed here (service layer) per the API-vs-Model
 - separation convention, not in the API modules.
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

import           Data.OpEnergy.API.V1.Natural(Natural, fromNatural)
import           Data.OpEnergy.API.V1.Block(BlockHeight)
import qualified Data.OpEnergy.Account.API.V1.Account as AccountAPI
import qualified Data.OpEnergy.Account.API.V1.UUID as AccountAPI
import           Data.OpEnergy.Offer.API.V1.OfferStatus
                 ( OfferStatus(..)
                 )
import           Data.OpEnergy.Offer.API.V1.OfferSide
                 ( OfferSide(..)
                 )
import           Data.OpEnergy.Offer.API.V1.ContractStatus
                 ( ContractStatus(..)
                 )
import qualified Data.OpEnergy.Offer.API.V1.OfferID as API
import qualified Data.OpEnergy.Offer.API.V1.OfferInfo as API
import qualified Data.OpEnergy.Offer.API.V1.ContractInfo as CAPI
import qualified Data.OpEnergy.Offer.API.V1.Constants as C
import           Data.Text.Show(tshow)

-- PersistField instances for OfferStatus -- placed here (service layer)
-- per the API-vs-Model separation convention, not in the API module.
instance PersistField OfferStatus where
  toPersistValue Open      = toPersistValue ("open" :: Text)
  toPersistValue Filled    = toPersistValue ("filled" :: Text)
  toPersistValue Expired   = toPersistValue ("expired" :: Text)
  toPersistValue Cancelled = toPersistValue ("cancelled" :: Text)
  fromPersistValue (PersistText "open")      = Right Open
  fromPersistValue (PersistText "filled")    = Right Filled
  fromPersistValue (PersistText "expired")   = Right Expired
  fromPersistValue (PersistText "cancelled") = Right Cancelled
  fromPersistValue _ = Left "fromPersistValue OfferStatus: unknown status"
instance PersistFieldSql OfferStatus where
  sqlType _ = SqlString

-- | PersistField for OfferSide — stored as uppercase text
instance PersistField OfferSide where
  toPersistValue Before = toPersistValue ("BEFORE" :: Text)
  toPersistValue After  = toPersistValue ("AFTER" :: Text)
  fromPersistValue (PersistText "BEFORE") = Right Before
  fromPersistValue (PersistText "AFTER")  = Right After
  fromPersistValue _ = Left "fromPersistValue OfferSide: unknown side"
instance PersistFieldSql OfferSide where
  sqlType _ = SqlString

-- | PersistField for ContractStatus — stored as lowercase text
instance PersistField ContractStatus where
  toPersistValue Live    = toPersistValue ("live" :: Text)
  toPersistValue Settled = toPersistValue ("settled" :: Text)
  fromPersistValue (PersistText "live")    = Right Live
  fromPersistValue (PersistText "settled") = Right Settled
  fromPersistValue _ = Left "fromPersistValue ContractStatus: unknown status"
instance PersistFieldSql ContractStatus where
  sqlType _ = SqlString

share [mkPersist sqlSettings, mkMigrate "migrateOffer"] [persistLowerCase|

-- | one row per offer group
Offer
  personUUID (AccountAPI.UUID AccountAPI.Person)
  creatorDisplayName AccountAPI.DisplayName
  targetBlock (Natural Int)
  mtpCutoffEpoch Word64
  side OfferSide
  validTillBlock (Natural Int)
  makerStakeSats Word64
  takerStakeSats Word64
  blockRate Double
  totalContracts (Natural Int)
  matchedCount (Natural Int)
  createdAtBlock (Natural Int)
  status OfferStatus
  expiresAt UTCTime Maybe
  refundedAt UTCTime Maybe
  created UTCTime
  deriving Eq Show

-- | one row per taker acceptance of an offer slot
Contract
  offerId OfferId
  targetBlock (Natural Int)
  mtpCutoffEpoch Word64
  makerSide OfferSide
  makerUUID (AccountAPI.UUID AccountAPI.Person)
  makerDisplayName AccountAPI.DisplayName
  makerStakeSats Word64
  takerUUID (AccountAPI.UUID AccountAPI.Person)
  takerDisplayName AccountAPI.DisplayName
  takerStakeSats Word64
  blockRate Double
  status ContractStatus
  actualMtpEpoch Word64 Maybe
  winnerSide OfferSide Maybe
  createdAtBlock (Natural Int)
  matchedAt UTCTime
  settledAt UTCTime Maybe
  deriving Eq Show
|]

-- | Model -> API glue for Offer
offerInfoFrom :: Text -> Offer -> API.OfferInfo
offerInfoFrom idText Offer{..} = API.OfferInfo
  { API.offerId = API.OfferID idText
  , API.creatorDisplayName = offerCreatorDisplayName
  , API.targetBlock = offerTargetBlock
  , API.mtpCutoffEpoch = offerMtpCutoffEpoch
  , API.side = offerSide
  , API.validTillBlock = offerValidTillBlock
  , API.makerStakeSats = offerMakerStakeSats
  , API.takerStakeSats = offerTakerStakeSats
  , API.blockRate = offerBlockRate
  , API.totalContracts = fromIntegral (fromNatural offerTotalContracts)
  , API.matchedCount = fromIntegral (fromNatural offerMatchedCount)
  , API.createdAtBlock = offerCreatedAtBlock
  , API.status = offerStatus
  , API.expiresAt = offerExpiresAt
  , API.refundedAt = offerRefundedAt
  , API.created = offerCreated
  }

-- | same as 'offerInfoFrom', for an 'Entity Offer'
offerInfoFromEntity :: Entity Offer -> API.OfferInfo
offerInfoFromEntity (Entity key offerVal) = offerInfoFrom (tshow (fromSqlKey key)) offerVal

-- | Model -> API glue for Contract.
-- @mYourRole@ is @Just "maker"@ or @Just "taker"@ when the
-- requesting user is a party; @Nothing@ for unauthenticated views.
-- @mTip@ is the current chain tip for computing confirmations.
contractInfoFromEntity
  :: Maybe Text
  -> Maybe BlockHeight
  -> Entity Contract
  -> CAPI.ContractInfo
contractInfoFromEntity mYourRole mTip (Entity key Contract{..}) =
  let idText = tshow (fromSqlKey key)
      offerIdText = tshow (fromSqlKey contractOfferId)
      confs = case (contractStatus, mTip) of
        (Live, Just tip) ->
          let tipInt = fromNatural tip
              tgtInt = fromNatural contractTargetBlock
          in fromIntegral (max 0 (tipInt - tgtInt + 1))
        (Settled, _) -> C.settlementConfirmations
        _ -> 0
  in CAPI.ContractInfo
    { CAPI.contractId = CAPI.ContractID idText
    , CAPI.offerId = API.OfferID offerIdText
    , CAPI.targetBlock = contractTargetBlock
    , CAPI.mtpCutoffEpoch = contractMtpCutoffEpoch
    , CAPI.makerSide = contractMakerSide
    , CAPI.makerDisplayName = contractMakerDisplayName
    , CAPI.makerStakeSats = contractMakerStakeSats
    , CAPI.takerDisplayName = contractTakerDisplayName
    , CAPI.takerStakeSats = contractTakerStakeSats
    , CAPI.blockRate = contractBlockRate
    , CAPI.status = contractStatus
    , CAPI.confirmations = confs
    , CAPI.actualMtpEpoch = contractActualMtpEpoch
    , CAPI.winnerSide = contractWinnerSide
    , CAPI.yourRole = mYourRole
    , CAPI.createdAtBlock = contractCreatedAtBlock
    , CAPI.matchedAt = contractMatchedAt
    , CAPI.settledAt = contractSettledAt
    }
