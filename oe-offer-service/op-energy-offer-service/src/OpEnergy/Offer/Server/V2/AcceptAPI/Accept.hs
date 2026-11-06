{-- | POST /api/v1/offer/:id/accept
 -}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE RecordWildCards            #-}
module OpEnergy.Offer.Server.V2.AcceptAPI.Accept
  ( accept
  , acceptHandler
  ) where

import           Control.Monad(unless, when)
import           Data.Maybe(isNothing)
import           Control.Monad.Trans.Reader(ask)
import           Control.Monad.Trans(lift)
import           Control.Monad.Trans.Except(ExceptT(..), throwE)
import           Control.Monad.IO.Class(liftIO)
import           Control.Monad.Logger(logError)
import           Data.Text(Text)

import           Database.Persist.Postgresql

import qualified Data.OpEnergy.Account.API.V1.Account as AccountAPI
import qualified Data.OpEnergy.Account.API.V2.WhoAmIResult as AccountV2
import           Data.OpEnergy.API.V1.Natural(verifyNatural, fromNatural)
import           Data.OpEnergy.Offer.API.V1.OfferID(OfferID(..))
import           Data.OpEnergy.Offer.API.V1.OfferStatus(OfferStatus(..))
import           Data.OpEnergy.Offer.API.V1.ContractStatus(ContractStatus(..))
import           Data.OpEnergy.Offer.API.V1.ContractInfo(ContractInfo)
import           Data.OpEnergy.Offer.API.V1.LiveMessage(LiveMessage(..))
import qualified Control.Concurrent.STM.TVar as TVar

import           OpEnergy.Offer.Server.V1.Time(getCurrentTimeDB)
import           Data.Time.Clock(addUTCTime)
import           OpEnergy.Offer.Server.V1.Class(AppM, State(..), profile, runLogging)
import           OpEnergy.Offer.Server.V1.Config(Config(..))
import qualified OpEnergy.Offer.Server.V1.AccountClient as AccountClient
import           Data.OpEnergy.Account.API.V1.Sats(Sats(..))
import           Data.OpEnergy.Account.API.V2.BalanceAdjustResult
                 ( BalanceAdjustResult(..)
                 )
import           Data.OpEnergy.Account.API.V2.FundsKind(FundsKind(..))
import           Data.OpEnergy.Account.API.V2.LedgerReason(LedgerReason(..))
import           OpEnergy.Offer.Server.V1.Offer
import           OpEnergy.Offer.Server.V1.OfferService(isOfferAcceptableAt)
import           OpEnergy.Offer.Server.V1.LiveEvent(LiveEvent(..))
import           OpEnergy.Offer.Server.V1.WebSocketService
                   ( publishLiveEvent
                   , withLiveEventOrderE
                   )

import           OpEnergy.Error
                   ( eitherThrowJSON, runExceptPrefixT
                   , exceptTMaybeT
                   , CallstackError, invalidRequest
                   , offerNotFound, offerNotOpen, offerFilled
                   , offerExpired, offerChanged, cannotAcceptOwnOffer
                   , tooManyFreeSatsContractsWithSameAccount
                   , chainTipUnknown
                   )

-- | Servant-facing handler
acceptHandler :: OfferID -> AccountAPI.AccountToken -> AppM ContractInfo
acceptHandler (OfferID idText) token =
  let name = "V2.AcceptAPI.Accept.acceptHandler"
  in profile name $ eitherThrowJSON (runLogging . $(logError)) $ accept idText token

-- | business logic for accept — creates one Contract, increments
-- matchedCount, and transitions the offer to Filled when full.
accept :: Text -> AccountAPI.AccountToken -> AppM (Either CallstackError ContractInfo)
accept idText token =
  let name = "V2.AcceptAPI.Accept.accept"
  in profile name $ runExceptPrefixT name $ do
  key <- exceptTMaybeT (invalidRequest "invalid offer id")
    $ return (offerKeyFromIDText idText)

  (AccountV2.WhoAmIResult takerUUIDV takerDisplayNameV _balance
    takerFreeBalance) <-
    ExceptT $ AccountClient.verifyAccountToken token

  State{ offerDBPool = pool, currentTip = currentTipV } <- lift ask
  offerVal <- exceptTMaybeT offerNotFound
    $! liftIO $ flip runSqlPersistMPool pool $ get key

  when (offerStatus offerVal /= Open) $ throwE offerNotOpen
  when (fromNatural (offerMatchedCount offerVal) >= fromNatural (offerTotalContracts offerVal)) $ throwE offerFilled
  when (offerPersonUUID offerVal == takerUUIDV) $ throwE cannotAcceptOwnOffer
  -- Two accounts, which were both given free sats, turn them into money by
  -- taking opposite sides of their own contracts: whichever of them wins,
  -- the profit on top of the stake is real sats and the pair keeps the
  -- rest. It is the same two accounts meeting again and again, so a pair,
  -- which has already shared the contracts it is allowed within the window,
  -- is refused while either side would be staking free sats. A contract,
  -- where neither side stakes them, is never refused by this, although it
  -- does count towards the window: what is bounded is how often these two
  -- accounts have met, not which pocket each meeting was paid from
  State{ config = Config{ configFreeSatsPairMatchLimit = pairLimit
                        , configFreeSatsPairMatchWindowSecs = pairWindowSecs
                        } } <- lift ask
  let takerStakesFreeSats =
        takerFreeBalance >= Sats (offerTakerStakeSats offerVal)
  when ( pairLimit > 0
      && (offerFundsKind offerVal == FreeSats || takerStakesFreeSats)) $ do
    nowUTC <- getCurrentTimeDB
    let since = addUTCTime (negate (fromIntegral pairWindowSecs)) nowUTC
    shared <- liftIO $ flip runSqlPersistMPool pool $ count
      (   [ ContractMakerUUID ==. offerPersonUUID offerVal
          , ContractTakerUUID ==. takerUUIDV
          , ContractMatchedAt >=. since
          ]
      ||. [ ContractMakerUUID ==. takerUUIDV
          , ContractTakerUUID ==. offerPersonUUID offerVal
          , ContractMatchedAt >=. since
          ]
      )
    when (fromIntegral shared >= pairLimit)
      $ throwE tooManyFreeSatsContractsWithSameAccount
  -- The expiry sweep closes an offer, which can't be accepted anymore, but
  -- it runs once per scheduler tick, so it may not have run since the tip
  -- moved. Without a known tip, this can't be checked.
  tip <- exceptTMaybeT chainTipUnknown $ liftIO $ TVar.readTVarIO currentTipV
  unless (isOfferAcceptableAt tip offerVal) $ throwE offerExpired

  -- balances and offers change from here on: in withLiveEventOrder, so the
  -- change's events are published in the order of the changes
  withLiveEventOrderE $ do
    BalanceAdjustResult (Sats takerBalance) takerFundsKindV <- ExceptT
      $ AccountClient.deductBalance takerUUIDV
        (Sats (offerTakerStakeSats offerVal)) Stake (Just (offerReference key))

    now <- getCurrentTimeDB
    let contractRow = Contract
          { contractOfferId = key
          , contractTargetBlock = offerTargetBlock offerVal
          , contractMtpCutoffEpoch = offerMtpCutoffEpoch offerVal
          , contractMakerSide = offerSide offerVal
          , contractMakerUUID = offerPersonUUID offerVal
          , contractMakerDisplayName = offerCreatorDisplayName offerVal
          , contractMakerStakeSats = offerMakerStakeSats offerVal
          , contractMakerFundsKind = offerFundsKind offerVal
          , contractTakerUUID = takerUUIDV
          , contractTakerDisplayName = takerDisplayNameV
          , contractTakerStakeSats = offerTakerStakeSats offerVal
          , contractTakerFundsKind = takerFundsKindV
          , contractBlockRate = offerBlockRate offerVal
          , contractStatus = Live
          , contractActualMtpEpoch = Nothing
          , contractWinnerSide = Nothing
          , contractCreatedAtBlock = tip
          , contractMatchedAt = now
          , contractSettledAt = Nothing
          }

    -- Atomic: conditional increment + insert contract to prevent
    -- two concurrent accepts from overselling the last slot.
    let staleCount = offerMatchedCount offerVal
        newCount   = verifyNatural (fromNatural staleCount + 1)
        isFilled   =
          fromNatural newCount >= fromNatural (offerTotalContracts offerVal)
        -- the offer as the transaction below leaves it
        acceptedVal = offerVal
          { offerMatchedCount = newCount
          , offerStatus = if isFilled then Filled else Open
          }
    mcontractKey <- liftIO $ flip runSqlPersistMPool pool $ do
      -- conditional update — only succeeds if matchedCount has not
      -- changed since we read it above
      bumped <- updateWhereCount
        [ OfferId ==. key
        , OfferMatchedCount ==. staleCount
        , OfferStatus ==. Open
        ]
        [ OfferMatchedCount =. newCount ]
      if bumped /= 1
        then return Nothing -- the slot is not ours: create no contract
        else do
          cKey <- insert contractRow
          -- transition to Filled when all contracts are matched
          when isFilled $
            updateWhere
              [ OfferId ==. key ]
              [ OfferStatus =. Filled ]
          return (Just cKey)

    -- if the conditional update matched 0 rows, another accept, a cancel or
    -- the expiry sweep changed the offer first -- refund and report which
    when (isNothing mcontractKey) $ do
      _ <- lift $ AccountClient.creditBalance takerUUIDV
        (Sats (offerTakerStakeSats offerVal))
        (Sats (offerTakerStakeSats offerVal)) takerFundsKindV Refund
        (Just (offerReference key))
      mlatest <- liftIO $ flip runSqlPersistMPool pool $ get key
      throwE $ case offerStatus <$> mlatest of
        Just Open -> offerChanged -- another accept took a slot: try again
        Just Filled -> offerFilled
        Just Expired -> offerExpired
        Just Cancelled -> offerNotOpen
        Nothing -> offerNotFound
    contractKey <- exceptTMaybeT offerFilled $ return mcontractKey

    let contractEntity = Entity contractKey contractRow
    -- sent to every connection, so without yourRole
    lift $ publishLiveEvent $! LiveEvent
      (LiveMessageContractCreated
        (contractInfoFromEntity Nothing (Just tip) contractEntity)
      )
      -- the maker's balance does not change on accept
      [(takerUUIDV, takerBalance)]
    lift $ publishLiveEvent $! LiveEvent
      (LiveMessageOfferChanged (offerInfoFromEntity (Entity key acceptedVal)))
      []
    return $! contractInfoFromEntity (Just "taker") (Just tip)
      contractEntity
