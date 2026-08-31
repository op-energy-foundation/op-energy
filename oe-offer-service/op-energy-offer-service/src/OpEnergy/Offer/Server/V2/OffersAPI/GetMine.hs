{-- | GET /api/v2/offer/mine
 -}
{-# LANGUAGE TemplateHaskell            #-}
module OpEnergy.Offer.Server.V2.OffersAPI.GetMine
  ( getMine
  , getMineHandler
  ) where

import           Control.Monad.Trans.Reader(ask)
import           Control.Monad.Trans(lift)
import           Control.Monad.Trans.Except(ExceptT(..))
import           Control.Monad.IO.Class(liftIO)
import           Control.Monad.Logger(logError)

import           Database.Persist.Postgresql

import qualified Data.OpEnergy.Account.API.V1.Account as AccountAPI
import qualified Data.OpEnergy.Account.API.V2.WhoAmIResult as AccountV2
import           Data.OpEnergy.Offer.API.V2.OffersAPI(OfferInfo)

import           OpEnergy.Offer.Server.V1.Class(AppM, State(..), profile, runLogging)
import qualified OpEnergy.Offer.Server.V1.AccountClient as AccountClient
import           OpEnergy.Offer.Server.V1.Offer

import           OpEnergy.Error(eitherThrowJSON, runExceptPrefixT, CallstackError)

getMineHandler :: AccountAPI.AccountToken -> AppM [OfferInfo]
getMineHandler token =
  let name = "V2.OffersAPI.GetMine.getMineHandler"
  in profile name $ eitherThrowJSON (runLogging . $(logError)) $ getMine token

getMine :: AccountAPI.AccountToken -> AppM (Either CallstackError [OfferInfo])
getMine token =
  let name = "getMine"
  in profile name $ runExceptPrefixT name $ do
  (AccountV2.WhoAmIResult personUUIDV _displayName _balance) <- ExceptT $ AccountClient.verifyAccountToken token
  State{ offerDBPool = pool } <- lift ask
  rows <- liftIO $ flip runSqlPersistMPool pool
    $ selectList [ OfferPersonUUID ==. personUUIDV ] [ Desc OfferCreated ]
  return $! map offerInfoFromEntity rows
