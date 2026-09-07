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
import           Control.Exception.Safe(SomeException)
import qualified Control.Exception.Safe as E
import           Data.Time.Clock(getCurrentTime)

import           Database.Persist.Postgresql

import qualified Data.OpEnergy.Account.API.V1.Account as AccountAPI
import qualified Data.OpEnergy.Account.API.V2.WhoAmIResult as AccountV2
import           Data.OpEnergy.API.V1.Natural(verifyNatural)
import           Data.OpEnergy.Offer.API.V1.OfferInfo(PostOfferRequest(..), PostOfferResult(..))
import qualified Data.OpEnergy.Offer.API.V1.Constants as C
import           Data.Text.Show(tshow)

import           OpEnergy.Offer.Server.V1.Class(AppM, State(..), profile, runLogging)
import qualified OpEnergy.Offer.Server.V1.AccountClient as AccountClient
import           Data.OpEnergy.Account.API.V1.Sats(Sats(..))
import           OpEnergy.Offer.Server.V1.Offer(Offer(..), offerInfoFrom)
import           Data.OpEnergy.Offer.API.V1.OfferStatus(OfferStatus(..))

import           OpEnergy.Error
                   ( eitherThrowJSON, runExceptPrefixT, describeError
                   , CallstackError, invalidRequest, unspecified
                   )

postHandler :: AccountAPI.AccountToken -> PostOfferRequest -> AppM PostOfferResult
postHandler token request =
  let name = "V2.PostOfferAPI.Post.postHandler"
  in profile name $ eitherThrowJSON (runLogging . $(logError)) $ post token request

post :: AccountAPI.AccountToken -> PostOfferRequest -> AppM (Either CallstackError PostOfferResult)
post token PostOfferRequest{..} =
  let name = "post"
  in profile name $ runExceptPrefixT name $ do
  when (totalContracts < 1 || totalContracts > C.maxContracts) $
    throwE $ invalidRequest ("totalContracts must be between 1 and " <> tshow C.maxContracts)
  when (makerStakeSats < C.minStakeSats || makerStakeSats > C.maxStakeSats) $
    throwE $ invalidRequest ("makerStakeSats must be between " <> tshow C.minStakeSats <> " and " <> tshow C.maxStakeSats)
  when (blockRate <= 0) $
    throwE $ invalidRequest "blockRate must be positive"

  (AccountV2.WhoAmIResult personUUIDV displayNameV _balance) <-
    ExceptT $ AccountClient.verifyAccountToken token

  State{ currentTip = currentTipV } <- lift ask
  mTip <- liftIO $ TVar.readTVarIO currentTipV
  case mTip of
    Just tip | targetBlock <= tip ->
      throwE $ invalidRequest ("targetBlock must be in the future (current tip: " <> tshow tip <> ")")
    _ -> return ()

  let totalStake = makerStakeSats * totalContracts
      takerStakeSatsV = C.totalPotSats - makerStakeSats
  _ <- ExceptT $ AccountClient.deductBalance personUUIDV (Sats totalStake)

  now <- liftIO getCurrentTime
  State{ offerDBPool = pool } <- lift ask
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
  einserted <- liftIO $ E.handle (\(e :: SomeException) -> return $! Left (tshow e))
    $ fmap Right $ flip runSqlPersistMPool pool $ insert offerRow
  case einserted of
    Right key -> return $! PostOfferResult
      { offers = [ offerInfoFrom (tshow (fromSqlKey key)) offerRow ] }
    Left insertErr -> do
      ecredited <- lift $ AccountClient.creditBalance personUUIDV (Sats totalStake)
      lift $ runLogging $ $(logError)
        ( "post: failed to persist offer row after staking " <> tshow totalStake
        <> " sats for " <> tshow personUUIDV <> ": " <> insertErr
        <> case ecredited of
             Right _ -> "; stake was refunded"
             Left creditErr -> "; stake refund ALSO failed, needs manual reconciliation: " <> describeError creditErr
        )
      throwE $ unspecified ("post: failed to persist offer row: " <> insertErr)
