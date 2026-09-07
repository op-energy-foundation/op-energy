{-- | POST /api/v1/offer/:id/accept
 -}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE RecordWildCards            #-}
module OpEnergy.Offer.Server.V2.AcceptAPI.Accept
  ( accept
  , acceptHandler
  ) where

import           Control.Monad(when)
import           Control.Monad.Trans.Reader(ask)
import           Control.Monad.Trans(lift)
import           Control.Monad.Trans.Except(ExceptT(..), throwE)
import           Control.Monad.IO.Class(liftIO)
import           Control.Monad.Logger(logError)
import           Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import           Data.Time.Clock(getCurrentTime)
import           Data.Int(Int64)

import           Database.Persist.Postgresql

import qualified Data.OpEnergy.Account.API.V1.Account as AccountAPI
import qualified Data.OpEnergy.Account.API.V2.WhoAmIResult as AccountV2
import           Data.OpEnergy.API.V1.Natural(verifyNatural, fromNatural)
import           Data.OpEnergy.Offer.API.V1.OfferID(OfferID(..))
import           Data.OpEnergy.Offer.API.V1.OfferStatus(OfferStatus(..))
import           Data.OpEnergy.Offer.API.V1.ContractStatus(ContractStatus(..))
import           Data.OpEnergy.Offer.API.V1.ContractInfo(ContractInfo)
import qualified Control.Concurrent.STM.TVar as TVar

import           OpEnergy.Offer.Server.V1.Class(AppM, State(..), profile, runLogging)
import qualified OpEnergy.Offer.Server.V1.AccountClient as AccountClient
import           Data.OpEnergy.Account.API.V1.Sats(Sats(..))
import           OpEnergy.Offer.Server.V1.Offer
                 ( Offer(..), OfferId, Contract(..)
                 , contractInfoFromEntity
                 )

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
  key <- case TR.decimal idText of
    Right (n, rest) | T.null rest -> return (toSqlKey n :: OfferId)
    _ -> throwE $ invalidRequest "invalid offer id"

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

  -- Atomic: insert contract + conditional increment to prevent
  -- two concurrent accepts from overselling the last slot.
  let staleCount = offerMatchedCount offerVal
      newCount   = verifyNatural (fromNatural staleCount + 1)
  (contractKey, updated) <- liftIO $ flip runSqlPersistMPool pool $ do
    -- conditional update — only succeeds if matchedCount has not
    -- changed since we read it above
    bumped <- updateWhereCount
      [ OfferId ==. key
      , OfferMatchedCount ==. staleCount
      , OfferStatus ==. Open
      ]
      [ OfferMatchedCount =. newCount ]
    cKey <- insert contractRow
    -- transition to Filled when all contracts are matched
    when (fromNatural newCount >= fromNatural (offerTotalContracts offerVal)) $ do
      _ <- (updateWhereCount
        [ OfferId ==. key ]
        [ OfferStatus =. Filled ] :: ReaderT SqlBackend IO Int64)
      return ()
    return (cKey, bumped)

  -- if the conditional update matched 0 rows, another accept won
  -- the race — refund and report filled
  when (updated /= 1) $ do
    _ <- lift $ AccountClient.creditBalance takerUUIDV (Sats (offerTakerStakeSats offerVal))
    throwE offerFilled

  return $! contractInfoFromEntity (Just "taker") mTip (Entity contractKey contractRow)
