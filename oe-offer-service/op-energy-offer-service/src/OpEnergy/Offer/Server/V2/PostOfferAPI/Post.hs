{-- | POST /api/v1/offer/post
 -}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module OpEnergy.Offer.Server.V2.PostOfferAPI.Post
  ( post
  , postHandler
  ) where

import           Control.Monad(when)
import           Control.Monad.Trans.Reader(ask)
import           Control.Monad.Trans(lift)
import           Control.Monad.Trans.Except(ExceptT(..), throwE)
import           Control.Monad.IO.Class(liftIO)
import           Control.Monad.Logger(logError)
import qualified Control.Concurrent.STM.TVar as TVar

import           Database.Persist.Postgresql

import qualified Data.OpEnergy.Account.API.V1.Account as AccountAPI
import qualified Data.OpEnergy.Account.API.V2.WhoAmIResult as AccountV2
import           Data.OpEnergy.API.V1.Natural(verifyNatural)
import           Data.OpEnergy.Offer.API.V1.OfferInfo(PostOfferRequest(..), PostOfferResult(..))
import qualified Data.OpEnergy.Offer.API.V1.Constants as C
import           Data.OpEnergy.Offer.API.V1.LiveMessage(LiveMessage(..))
import           Data.Text.Show(tshow)

import           OpEnergy.Offer.Server.V1.Time(getCurrentTimeDB)
import           OpEnergy.Offer.Server.V1.Class
                   ( AppM, State(..), profile, runLogging, withDBTransaction
                   )
import           OpEnergy.Offer.Server.V1.Config(Config(..))
import qualified OpEnergy.Offer.Server.V1.AccountClient as AccountClient
import           Data.OpEnergy.Account.API.V1.Sats(Sats(..))
import           OpEnergy.Offer.Server.V1.Offer
                 ( Offer(..), EntityField(..), offerInfoFromEntity
                 )
import           Data.OpEnergy.Account.API.V2.LedgerReason (LedgerReason(..))
import           OpEnergy.Offer.Server.V1.LiveEvent(LiveEvent(..))
import           OpEnergy.Offer.Server.V1.WebSocketService
                   ( publishLiveEvent
                   , withLiveEventOrderE
                   )
import           Data.OpEnergy.Offer.API.V1.OfferStatus(OfferStatus(..))

import           OpEnergy.Error
                   ( eitherThrowJSON, runExceptPrefixT, describeError
                   , CallstackError, invalidRequest
                   , exceptTMaybeT, chainTipUnknown
                   )

postHandler :: AccountAPI.AccountToken -> PostOfferRequest -> AppM PostOfferResult
postHandler token request =
  let name = "V2.PostOfferAPI.Post.postHandler"
  in profile name $ eitherThrowJSON (runLogging . $(logError)) $ post token request

post :: AccountAPI.AccountToken -> PostOfferRequest -> AppM (Either CallstackError PostOfferResult)
post token PostOfferRequest{..} =
  let name = "V2.PostOfferAPI.Post.post"
  in profile name $ runExceptPrefixT name $ do
  when (totalContracts < 1 || totalContracts > C.maxContracts) $
    throwE $ invalidRequest ("totalContracts must be between 1 and " <> tshow C.maxContracts)
  State{ config = Config{ configPlatformFeeSats = platformFeeSats } } <- lift ask
  -- the fee comes out of the pot before the winner is paid, so a maker
  -- stake, which does not cover the fee, leaves the taker's side nothing to
  -- win: the taker's stake is the rest of the pot, which is then as large as
  -- the payout. The smallest stake above that is the fee plus minStakeSats
  let minStakeSats = platformFeeSats + C.minStakeSats
  when (makerStakeSats < minStakeSats || makerStakeSats > C.maxStakeSats) $
    throwE $ invalidRequest ("makerStakeSats must be between " <> tshow minStakeSats <> " and " <> tshow C.maxStakeSats)
  when (blockRate <= 0) $
    throwE $ invalidRequest "blockRate must be positive"

  (AccountV2.WhoAmIResult personUUIDV displayNameV _balance _role) <-
    ExceptT $ AccountClient.verifyAccountToken token

  State{ currentUnconfirmedTip = currentUnconfirmedTipV } <- lift ask
  -- the tip is forgotten while the connection to blockspan service is down,
  -- and an offer, whose target block is mined already, must not be posted
  -- in the meantime: accept refuses without a tip, and so does this
  tip <- exceptTMaybeT chainTipUnknown
    $ liftIO $ TVar.readTVarIO currentUnconfirmedTipV
  when (targetBlock <= tip) $ throwE $ invalidRequest
    ("targetBlock must be in the future (current tip: " <> tshow tip <> ")")

  -- balances and offers change from here on: in withLiveEventOrder, so the
  -- change's events are published in the order of the changes
  withLiveEventOrderE $ do
    let totalStake = makerStakeSats * totalContracts
        takerStakeSatsV = C.totalPotSats - makerStakeSats
    makerBalance <-
      ExceptT $ AccountClient.deductBalance personUUIDV (Sats totalStake)
        Stake Nothing -- the offer has no id until it is inserted below

    now <- getCurrentTimeDB
    let offerRow = Offer
          { offerPersonUUID = personUUIDV
          , offerCreatorDisplayName = displayNameV
          , offerTargetBlock = targetBlock
          , offerMtpCutoffEpoch = mtpCutoffEpoch
          , offerSide = side
          , offerValidTillBlock = validTillBlock
          , offerMakerStakeSats = makerStakeSats
          , offerTakerStakeSats = takerStakeSatsV
          , offerBlockRate = blockRate
          , offerTotalContracts = verifyNatural (fromIntegral totalContracts)
          , offerMatchedCount = verifyNatural 0
          , offerCreatedAtBlock = createdAtBlock
          , offerStatus = Open
          , offerExpiresAt = Nothing
          , offerRefundedAt = Nothing
          , offerCreated = now
          }
    -- the stake is deducted by now, so a failed insert is answered only
    -- after it is refunded. withDBTransaction logs the query's own error
    -- and answers dbQueryError, which is reported as it is
    einserted <- lift $ withDBTransaction "insertOffer" $ insert offerRow
    case einserted of
      Right key -> do
        let offerInfo = offerInfoFromEntity (Entity key offerRow)
        lift $ publishLiveEvent $! LiveEvent
          (LiveMessageOfferCreated offerInfo)
          [(personUUIDV, makerBalance)]
        return $! PostOfferResult
          { offers = [ offerInfo ] }
      Left insertErr -> do
        -- an insert, which answers an error, has not always failed: a
        -- connection, which drops after the COMMIT, loses the answer
        -- while the row is there. That offer is open and staked for, so
        -- giving the stake back here would hand it over a second time
        -- when the offer is later cancelled or expires. The row is
        -- looked for by the account and the second it was created in,
        -- which together name the row of this request
        mexisting <- lift
          $ withDBTransaction "findOfferAfterFailedInsert" $ selectFirst
            [ OfferPersonUUID ==. personUUIDV
            , OfferCreated ==. now
            ]
            []
        case mexisting of
          -- the row is there, so the offer was made and it was the answer
          -- to the insert, which was lost. Answered as the offer it is,
          -- with the event the normal path publishes, rather than as a
          -- failure: a maker, which is told its post failed, posts again
          -- while this offer already holds its stake
          Right (Just existing) -> do
            lift $ runLogging $ $(logError)
              ( "post: the offer row is there although the insert answered "
              <> "an error, so it is answered as made and the stake of "
              <> tshow totalStake <> " sats for " <> tshow personUUIDV
              <> " stays with it: " <> describeError insertErr
              )
            let offerInfo = offerInfoFromEntity existing
            lift $ publishLiveEvent $! LiveEvent
              (LiveMessageOfferCreated offerInfo)
              [(personUUIDV, makerBalance)]
            return $! PostOfferResult { offers = [ offerInfo ] }
          -- only a row, which is known not to be there, is refunded for.
          -- Where this could not be read the stake is left where it is
          -- and written down: a stake, which was not given back, is one
          -- a person can put right, while one given back twice is sats
          -- this service invented
          Right Nothing -> do
            ecredited <- lift
              $ AccountClient.creditBalance personUUIDV (Sats totalStake)
                  Refund Nothing
            lift $ runLogging $ $(logError)
              ( "post: failed to persist offer row after staking "
              <> tshow totalStake
              <> " sats for " <> tshow personUUIDV <> ": "
              <> describeError insertErr
              <> case ecredited of
                   Right _ -> "; stake was refunded"
                   Left creditErr ->
                     "; stake refund ALSO failed, needs manual reconciliation: "
                     <> describeError creditErr
              )
            throwE insertErr
          Left readErr -> do
            lift $ runLogging $ $(logError)
              ( "post: the insert of the offer answered an error and "
              <> "whether the row is there could not be read, so the stake "
              <> "of " <> tshow totalStake <> " sats for "
              <> tshow personUUIDV <> " is left as it is and needs manual "
              <> "reconciliation: " <> describeError insertErr
              <> "; and " <> describeError readErr
              )
            throwE insertErr
