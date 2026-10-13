{-- | Contract settlement sweep, run per scheduler tick.
 -}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module OpEnergy.Offer.Server.V2.Settlement
  ( settleContracts
  ) where

import           Control.Monad (forM, when)
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Reader (ask)
import           Control.Monad.Trans.Except (ExceptT(..), throwE)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Logger (logError, logInfo)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Maybe (catMaybes)
import           Data.Word (Word64)

import           Database.Persist.Postgresql
import           Prometheus (MonadMonitor)

import           Data.OpEnergy.API.V1.Block (BlockHeight)
import           Data.OpEnergy.API.V1.Natural (verifyNatural, fromNatural)
import           Data.OpEnergy.Account.API.V1.Sats (Sats(..))
import           Data.OpEnergy.Offer.API.V1.ContractStatus (ContractStatus(..))
import           Data.OpEnergy.Offer.API.V1.OfferSide (OfferSide(..))
import qualified Data.OpEnergy.Offer.API.V1.Constants as C
import           Data.OpEnergy.Offer.API.V1.LiveMessage (LiveMessage(..))
import           Data.Text.Show (tshow)

import           OpEnergy.Offer.Server.V1.Time (getCurrentTimeDB)
import           OpEnergy.Offer.Server.V1.Class
                 ( AppT
                 , State(..)
                 , profile
                 , runLogging
                 , withDBTransaction
                 )
import           OpEnergy.Offer.Server.V1.Config (Config(..))
import           OpEnergy.Offer.Server.V1.Offer
import           OpEnergy.Offer.Server.V1.PlatformStats (addCollectedFeeTx)
import           OpEnergy.Offer.Server.V1.LiveEvent
                 ( LiveEvent(..)
                 , changedBalance
                 )
import           OpEnergy.Offer.Server.V1.WebSocketService
                 ( publishLiveEvent
                 , withLiveEventOrderE
                 )
import qualified OpEnergy.Offer.Server.V1.BlockspanClient as BlockspanClient
import qualified OpEnergy.Offer.Server.V1.AccountClient as AccountClient
import           OpEnergy.Error
                   ( runExceptPrefixT, describeError
                   , CallstackError, potDoesNotCoverFee
                   )

-- | Settles every live contract, whose target block has got
-- 'C.settlementConfirmations' confirmations at the given chain tip, ie
-- @targetBlock + settlementConfirmations <= tip@.
-- Returns the amount of contracts settled during this call, or the error,
-- which kept the sweep from looking for them at all, so its caller can tell
-- a tick, which found nothing to settle, from a tick, which could not look.
-- A single contract, which could not be settled, is logged and left live
-- for a later tick instead.
settleContracts
  :: (MonadIO m, MonadMonitor m)
  => BlockHeight
  -> AppT m (Either CallstackError Int)
settleContracts tipHeight =
    let name = "V2.Settlement.settleContracts"
    in profile name $ runExceptPrefixT name $ do
  -- the sweep has nothing to work from when this fails, so it stops here
  -- instead of reading a failed query as "no contract is settleable"
  contracts <- ExceptT $ selectSettleableContracts tipHeight
  mediantimes <- lift $ getTargetBlockMediantimes contracts
  settled <- lift $ forM contracts $ settleContractLogging mediantimes
  return $! length (filter id settled)

-- | returns live contracts, whose target block has got
-- 'C.settlementConfirmations' confirmations at the given chain tip
selectSettleableContracts
  :: (MonadIO m, MonadMonitor m)
  => BlockHeight
  -> AppT m (Either CallstackError [Entity Contract])
selectSettleableContracts tipHeight =
  let name = "V2.Settlement.selectSettleableContracts"
  in profile name $ runExceptPrefixT name $ do
  let maxTargetBlock =
        fromNatural tipHeight - fromIntegral C.settlementConfirmations
  if maxTargetBlock < 0
    then return [] -- chain is shorter than the confirmation depth
    else ExceptT $ withDBTransaction "selectList" $ selectList
      [ ContractStatus ==. Live
      , ContractTargetBlock <=. verifyNatural maxTargetBlock
      ]
      [ Asc ContractId ]

-- | mediantime of each distinct target block of the given contracts, as
-- reported by blockspan service's HTTP API. Every contract of one offer
-- carries the same target block and a sweep settles them together, so one
-- request per distinct block saves up to 'C.maxContracts' - 1 requests per
-- offer. A block, whose request failed, is logged and left out of the
-- result: the contracts, which target it, are settled on a later tick.
getTargetBlockMediantimes
  :: (MonadIO m, MonadMonitor m)
  => [Entity Contract]
  -> AppT m (Map BlockHeight Word64)
getTargetBlockMediantimes contracts =
    let name = "V2.Settlement.getTargetBlockMediantimes"
    in profile name $ do
  mentries <- forM targetBlocks $ \height -> either
    (\err -> do
      runLogging $ $(logError)
        ( "target block " <> tshow height <> ": " <> describeError err )
      return Nothing
    )
    (return . Just . (,) height)
    =<< BlockspanClient.getBlockMediantime height
  return $! Map.fromList (catMaybes mentries)
  where
    targetBlocks = Set.toList $! Set.fromList
      $! map (contractTargetBlock . entityVal) contracts

-- | settles the given contract with 'settleContract', logging whatever kept
-- it from being settled: the mediantime of its target block is missing from
-- the given mediantimes, or its settlement itself failed. The contract
-- stays live in both cases, so a later tick settles it.
settleContractLogging
  :: (MonadIO m, MonadMonitor m)
  => Map BlockHeight Word64
  -> Entity Contract
  -> AppT m Bool
settleContractLogging mediantimes contract@(Entity contractId contractVal) =
  -- a failed mediantime request is logged by getTargetBlockMediantimes
  maybe (return False) settleWithMediantime
    $! Map.lookup (contractTargetBlock contractVal) mediantimes
  where
    settleWithMediantime actualMtpEpoch = either
      (\err -> do
        -- the contract stays live and will be retried on the next tick
        runLogging $ $(logError)
          ( "contract " <> tshow (fromSqlKey contractId) <> ": "
          <> describeError err )
        return False
      )
      return
      =<< settleContract actualMtpEpoch contract

-- | settles the given contract:
--
-- - determines the winning side from the given mediantime of the contract's
--   target block: BEFORE wins if it is earlier than the contract's cutoff,
--   AFTER wins otherwise;
-- - in one transaction, marks the contract as settled and adds the platform
--   fee to the platform's total. The transaction changes nothing if the
--   contract is no longer live, so a contract is never paid twice;
-- - credits the winner with both stakes minus the platform fee;
-- - publishes the settled contract as 'LiveMessageContractSettled'.
-- Returns 'False' if the contract was no longer live.
settleContract
  :: (MonadIO m, MonadMonitor m)
  => Word64
     -- ^ mediantime of the contract's target block
  -> Entity Contract
  -> AppT m (Either CallstackError Bool)
settleContract actualMtpEpoch (Entity contractId contract@Contract{..}) =
  let name = "V2.Settlement.settleContract"
  in profile name $ runExceptPrefixT name $ do
  State{ config = Config{ configPlatformFeeSats = platformFeeSats } } <- lift ask
  let potSats = contractMakerStakeSats + contractTakerStakeSats
  when (potSats <= platformFeeSats) $
    throwE $ potDoesNotCoverFee potSats platformFeeSats
  let winnerSide = if actualMtpEpoch < contractMtpCutoffEpoch
        then Before
        else After
      winnerUUID = if winnerSide == contractMakerSide
        then contractMakerUUID
        else contractTakerUUID
      payoutSats = potSats - platformFeeSats
  -- the contract and the winner's balance change from here on: in
  -- withLiveEventOrder, so the change's events are published in order
  withLiveEventOrderE $ do
    now <- getCurrentTimeDB
    -- the contract as the transaction below leaves it: keep both in sync
    let settledContract = contract
          { contractStatus = Settled
          , contractWinnerSide = Just winnerSide
          , contractActualMtpEpoch = Just actualMtpEpoch
          , contractSettledAt = Just now
          }
    settled <- ExceptT $ withDBTransaction "markSettled" $ do
      updated <- updateWhereCount
        [ ContractId ==. contractId, ContractStatus ==. Live ]
        [ ContractStatus =. Settled
        , ContractWinnerSide =. Just winnerSide
        , ContractActualMtpEpoch =. Just actualMtpEpoch
        , ContractSettledAt =. Just now
        ]
      when (updated == 1) $ addCollectedFeeTx platformFeeSats now
      return $! updated == 1
    when settled $ do
      ecredited <- lift
        $ AccountClient.creditBalance winnerUUID (Sats payoutSats)
      lift $ runLogging $ case ecredited of
        Right _ -> $(logInfo)
          ( "contract " <> tshow (fromSqlKey contractId) <> " settled: "
          <> tshow winnerSide <> " won, " <> tshow payoutSats
          <> " sats credited to " <> tshow winnerUUID
          )
        Left err -> $(logError)
          ( "contract " <> tshow (fromSqlKey contractId) <> " settled ("
          <> tshow winnerSide <> " won) but its payout of " <> tshow payoutSats
          <> " sats was NOT credited to " <> tshow winnerUUID
          <> " -- creditBalance failed, needs manual reconciliation: "
          <> describeError err
          )
      -- sent to every connection, so without yourRole. No chain tip is
      -- needed: a settled contract's confirmations don't depend on it
      lift $ publishLiveEvent $! LiveEvent
        (LiveMessageContractSettled
          (contractInfoFromEntity Nothing Nothing
            (Entity contractId settledContract))
        )
        -- only the winner's balance has changed
        (changedBalance winnerUUID ecredited)
    return settled
