{-- | POST /api/v1/offer/:id/accept
 -}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE RecordWildCards            #-}
module OpEnergy.Offer.Server.V2.AcceptAPI.Accept
  ( accept
  , acceptHandler
  ) where

import           Control.Monad(when)
import           Data.Maybe(isNothing)
import           Control.Monad.Trans.Reader(ask)
import           Control.Monad.Trans(lift)
import           Control.Monad.Trans.Except(ExceptT(..), throwE)
import           Control.Monad.IO.Class(liftIO)
import           Control.Monad.Logger(logError)
import           Data.Text(Text)
import           Data.Time.Clock(getCurrentTime)

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

import           OpEnergy.Offer.Server.V1.Class(AppM, State(..), profile, runLogging)
import qualified OpEnergy.Offer.Server.V1.AccountClient as AccountClient
import           Data.OpEnergy.Account.API.V1.Sats(Sats(..))
import           OpEnergy.Offer.Server.V1.Offer
import           OpEnergy.Offer.Server.V1.LiveEvent(LiveEvent(..))
import           OpEnergy.Offer.Server.V1.WebSocketService(publishLiveEvent)

import           OpEnergy.Error
                   ( eitherThrowJSON, runExceptPrefixT
                   , exceptTMaybeT
                   , CallstackError, invalidRequest
                   , offerNotFound, offerNotOpen, offerFilled
                   , cannotAcceptOwnOffer
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

  (AccountV2.WhoAmIResult takerUUIDV takerDisplayNameV _balance) <-
    ExceptT $ AccountClient.verifyAccountToken token

  State{ offerDBPool = pool, currentTip = currentTipV } <- lift ask
  offerVal <- exceptTMaybeT offerNotFound
    $! liftIO $ flip runSqlPersistMPool pool $ get key

  when (offerStatus offerVal /= Open) $ throwE offerNotOpen
  when (fromNatural (offerMatchedCount offerVal) >= fromNatural (offerTotalContracts offerVal)) $ throwE offerFilled
  when (offerPersonUUID offerVal == takerUUIDV) $ throwE cannotAcceptOwnOffer

  _ <- ExceptT $ AccountClient.deductBalance takerUUIDV (Sats (offerTakerStakeSats offerVal))

  now <- liftIO getCurrentTime
  mTip <- liftIO $ TVar.readTVarIO currentTipV
  let takerCreatedAtBlock = case mTip of
        Just tip -> tip
        Nothing  -> offerCreatedAtBlock offerVal
  let contractRow = Contract
        { contractOfferId = key
        , contractTargetBlock = offerTargetBlock offerVal
        , contractMtpCutoffEpoch = offerMtpCutoffEpoch offerVal
        , contractMakerSide = offerSide offerVal
        , contractMakerUUID = offerPersonUUID offerVal
        , contractMakerDisplayName = offerCreatorDisplayName offerVal
        , contractMakerStakeSats = offerMakerStakeSats offerVal
        , contractTakerUUID = takerUUIDV
        , contractTakerDisplayName = takerDisplayNameV
        , contractTakerStakeSats = offerTakerStakeSats offerVal
        , contractBlockRate = offerBlockRate offerVal
        , contractStatus = Live
        , contractActualMtpEpoch = Nothing
        , contractWinnerSide = Nothing
        , contractCreatedAtBlock = takerCreatedAtBlock
        , contractMatchedAt = now
        , contractSettledAt = Nothing
        }

  -- Atomic: conditional increment + insert contract to prevent
  -- two concurrent accepts from overselling the last slot.
  let staleCount = offerMatchedCount offerVal
      newCount   = verifyNatural (fromNatural staleCount + 1)
      isFilled   = fromNatural newCount >= fromNatural (offerTotalContracts offerVal)
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

  -- if the conditional update matched 0 rows, another accept or a cancel
  -- won the race — refund and report filled
  when (isNothing mcontractKey) $ do
    _ <- lift $ AccountClient.creditBalance takerUUIDV (Sats (offerTakerStakeSats offerVal))
    throwE offerFilled
  contractKey <- exceptTMaybeT offerFilled $ return mcontractKey

  lift $ publishLiveEvent $! LiveEvent
    (LiveMessageContractCreated
      (contractIDFromKey contractKey)
      (OfferID idText)
    )
    [offerPersonUUID offerVal, takerUUIDV]
  lift $ publishLiveEvent $! LiveEvent
    (LiveMessageOfferChanged
      (OfferID idText)
      (if isFilled then Filled else Open)
      (fromIntegral (fromNatural newCount))
    )
    []
  return $! contractInfoFromEntity (Just "taker") mTip (Entity contractKey contractRow)
