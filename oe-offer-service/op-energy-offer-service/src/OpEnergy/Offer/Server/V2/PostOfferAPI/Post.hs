{-- | POST /api/v2/offer/post
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
import           Data.Word(Word64)

import           Database.Persist.Postgresql

import qualified Data.OpEnergy.Account.API.V1.Account as AccountAPI
import qualified Data.OpEnergy.Account.API.V2.WhoAmIResult as AccountV2
import           Data.OpEnergy.Offer.API.V1.OfferInfo(PostOfferRequest(..), PostOfferResult(..))
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

minOffers, maxOffers :: Word64
minOffers = 1
maxOffers = 20

minStakeSats, maxStakeSats :: Word64
minStakeSats = 1
maxStakeSats = 100000000

postHandler :: AccountAPI.AccountToken -> PostOfferRequest -> AppM PostOfferResult
postHandler token request =
  let name = "V2.PostOfferAPI.Post.postHandler"
  in profile name $ eitherThrowJSON (runLogging . $(logError)) $ post token request

post :: AccountAPI.AccountToken -> PostOfferRequest -> AppM (Either CallstackError PostOfferResult)
post token PostOfferRequest{..} =
  let name = "post"
  in profile name $ runExceptPrefixT name $ do
  when (numberOfOffers < minOffers || numberOfOffers > maxOffers) $
    throwE $ invalidRequest ("numberOfOffers must be between " <> tshow minOffers <> " and " <> tshow maxOffers)
  when (makerStakeSats < minStakeSats || makerStakeSats > maxStakeSats) $
    throwE $ invalidRequest ("makerStakeSats must be between " <> tshow minStakeSats <> " and " <> tshow maxStakeSats)

  (AccountV2.WhoAmIResult personUUIDV displayNameV _balance) <-
    ExceptT $ AccountClient.verifyAccountToken token

  State{ currentTip = currentTipV } <- lift ask
  mTip <- liftIO $ TVar.readTVarIO currentTipV
  case mTip of
    Just tip | targetBlock <= tip ->
      throwE $ invalidRequest ("targetBlock must be in the future (current tip: " <> tshow tip <> ")")
    _ -> return ()

  let totalStake = makerStakeSats * numberOfOffers
  _ <- ExceptT $ AccountClient.deductBalance personUUIDV (Sats totalStake)

  now <- liftIO getCurrentTime
  State{ offerDBPool = pool } <- lift ask
  let offerRow = Offer
        { offerPersonUUID = personUUIDV
        , offerCreatorDisplayName = displayNameV
        , offerTargetBlock = targetBlock
        , offerValidTillBlock = validTillBlock
        , offerMakerStakeSats = makerStakeSats
        , offerStatus = Open
        , offerExpiresAt = Nothing
        , offerRefundedAt = Nothing
        , offerCreated = now
        }
  einserted <- liftIO $ E.handle (\(e :: SomeException) -> return $! Left (tshow e))
    $ fmap Right $ flip runSqlPersistMPool pool $ sequence $ replicate (fromIntegral numberOfOffers) (insert offerRow)
  case einserted of
    Right keys -> return $! PostOfferResult
      { offers = map (\k -> offerInfoFrom (tshow (fromSqlKey k)) offerRow) keys }
    Left insertErr -> do
      ecredited <- lift $ AccountClient.creditBalance personUUIDV (Sats totalStake)
      lift $ runLogging $ $(logError)
        ( "post: failed to persist offer rows after staking " <> tshow totalStake
        <> " sats for " <> tshow personUUIDV <> ": " <> insertErr
        <> case ecredited of
             Right _ -> "; stake was refunded"
             Left creditErr -> "; stake refund ALSO failed, needs manual reconciliation: " <> describeError creditErr
        )
      throwE $ unspecified ("post: failed to persist offer rows: " <> insertErr)
