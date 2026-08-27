{-- | POST /api/v2/offer/post -- see Data.OpEnergy.Offer.API.V2.PostOfferAPI
 - for the wire types/route and docs/plans/post-offer-api.md for scope.
 -
 - Posts numberOfOffers identical (except id) maker offers for the account
 - identified by the AccountToken header, staking
 - numberOfOffers*makerStakeSats sats out of its sandbox balance -- which
 - lives on oe-account-service's Person row, not in this service's own DB
 - (see OpEnergy.Offer.Server.V1.AccountClient's module header). The order
 - here matters: deduct first (a single atomic conditional decrement on
 - oe-account-service's side -- "balance >= totalStake", not a
 - read-then-write, so two concurrent posts from the same account can't
 - both pass a balance check that's gone stale by the time either writes),
 - THEN insert the offer rows locally. If the local insert then fails --
 - this service's own DB being unreachable, most plausibly -- the stake is
 - credited back (best-effort) so a dead local DB doesn't silently eat
 - someone's sats; see the comment at that call site for what happens if
 - even that fails.
 -}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module OpEnergy.Offer.Server.V2.PostOfferAPI.Post
  ( post
  , postHandler
  ) where

import           Control.Monad(replicateM, when)
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
import qualified Data.OpEnergy.Account.API.V2 as AccountV2
import           Data.OpEnergy.Offer.API.V2.PostOfferAPI(PostOfferRequest(..), PostOfferResult(..))
import           Data.Text.Show(tshow)

import           OpEnergy.Offer.Server.V1.Class(AppM, State(..), profile, runLogging)
import qualified OpEnergy.Offer.Server.V1.AccountClient as AccountClient
import           OpEnergy.Offer.Server.V1.Offer(Offer(..), offerInfoFrom)
import           Data.OpEnergy.Offer.API.V1.OfferStatus(offerStatusOpen)

import           OpEnergy.Error
                   ( eitherThrowJSON, runExceptPrefixT, describeError
                   , CallstackError, invalidRequest, unspecified
                   )

-- | server-side re-validation of what the frontend's steppers/slider already
-- enforce client-side -- never trust client-side bounds alone.
minOffers, maxOffers :: Word64
minOffers = 1
maxOffers = 20

minStakeSats, maxStakeSats :: Word64
minStakeSats = 1
maxStakeSats = 100000000 -- 1 BTC in sats -- generous upper bound, not a real product limit

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
  -- deduct first: cheap to undo (a credit) if what follows fails, whereas
  -- inserting offer rows first and deducting after would mean deleting
  -- rows on a failed deduct instead -- same shape of problem, no
  -- meaningfully safer than this order.
  _ <- ExceptT $ AccountClient.deductBalance personUUIDV totalStake

  now <- liftIO getCurrentTime
  State{ offerDBPool = pool } <- lift ask
  let offerRow = Offer
        { offerPersonUUID = personUUIDV
        , offerCreatorDisplayName = displayNameV
        , offerTargetBlock = targetBlock
        , offerValidTillBlock = validTillBlock
        , offerMakerStakeSats = makerStakeSats
        , offerStatus = offerStatusOpen
        , offerExpiresAt = Nothing
        , offerRefundedAt = Nothing
        , offerCreated = now
        }
  einserted <- liftIO $ E.handle (\(e :: SomeException) -> return $! Left (tshow e))
    $ fmap Right $ flip runSqlPersistMPool pool $ replicateM numberOfOffers (insert offerRow)
  case einserted of
    Right keys -> return $! PostOfferResult
      { offers = map (\k -> offerInfoFrom (tshow (fromSqlKey k)) offerRow) keys }
    Left insertErr -> do
      -- the stake was already deducted (above) but never turned into
      -- offer rows -- credit it back so a dead local DB doesn't silently
      -- eat someone's sats. If even the credit-back fails, this is now the
      -- same "needs manual reconciliation" situation
      -- OpEnergy.Offer.Server.V1.OfferService.refundAndCloseOffer accepts
      -- for cancel/expire -- log loudly and surface the original error to
      -- the caller either way, since their offer(s) were never created.
      ecredited <- lift $ AccountClient.creditBalance personUUIDV totalStake
      lift $ runLogging $ $(logError)
        ( "post: failed to persist offer rows after staking " <> tshow totalStake
        <> " sats for " <> tshow personUUIDV <> ": " <> insertErr
        <> case ecredited of
             Right _ -> "; stake was refunded"
             Left creditErr -> "; stake refund ALSO failed, needs manual reconciliation: " <> describeError creditErr
        )
      throwE $ unspecified ("post: failed to persist offer rows: " <> insertErr)
