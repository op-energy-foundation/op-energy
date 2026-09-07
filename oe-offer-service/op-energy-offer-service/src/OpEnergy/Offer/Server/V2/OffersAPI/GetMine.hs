{-- | GET /api/v1/offer/mine
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
import qualified Control.Concurrent.STM.TVar as TVar

import           Database.Persist.Postgresql

import qualified Data.OpEnergy.Account.API.V1.Account as AccountAPI
import qualified Data.OpEnergy.Account.API.V2.WhoAmIResult as AccountV2
import           Data.OpEnergy.Offer.API.V1.OfferInfo(MyOffersResult(..))

import           OpEnergy.Offer.Server.V1.Class(AppM, State(..), profile, runLogging)
import qualified OpEnergy.Offer.Server.V1.AccountClient as AccountClient
import           OpEnergy.Offer.Server.V1.Offer

import           OpEnergy.Error(eitherThrowJSON, runExceptPrefixT, CallstackError)

getMineHandler :: AccountAPI.AccountToken -> AppM MyOffersResult
getMineHandler token =
  let name = "V2.OffersAPI.GetMine.getMineHandler"
  in profile name $ eitherThrowJSON (runLogging . $(logError)) $ getMine token

getMine :: AccountAPI.AccountToken -> AppM (Either CallstackError MyOffersResult)
getMine token =
  let name = "V2.OffersAPI.GetMine.getMine"
  in profile name $ runExceptPrefixT name $ do
  (AccountV2.WhoAmIResult personUUIDV _displayName _balance) <-
    ExceptT $ AccountClient.verifyAccountToken token
  State{ offerDBPool = pool, currentTip = currentTipV } <- lift ask
  mTip <- liftIO $ TVar.readTVarIO currentTipV
  (offerRows, contractRows) <- liftIO $ flip runSqlPersistMPool pool $ do
    os <- selectList [ OfferPersonUUID ==. personUUIDV ] [ Desc OfferCreated ]
    cs <- selectList
      ( [ ContractMakerUUID ==. personUUIDV ]
        ||. [ ContractTakerUUID ==. personUUIDV ]
      )
      [ Desc ContractMatchedAt ]
    return (os, cs)
  let assignRole entity =
        let c = entityVal entity
        in if contractMakerUUID c == personUUIDV
             then contractInfoFromEntity (Just "maker") mTip entity
             else contractInfoFromEntity (Just "taker") mTip entity
  return $! MyOffersResult
    { offers = map offerInfoFromEntity offerRows
    , contracts = map assignRole contractRows
    }
