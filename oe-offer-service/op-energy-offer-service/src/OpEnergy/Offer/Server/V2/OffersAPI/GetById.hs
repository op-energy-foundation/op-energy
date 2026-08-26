{-- | GET /api/v2/offer/:id -- full details for a single offer by id (its
 - stringified DB key). Public, unauthenticated, same as /list.
 -}
{-# LANGUAGE TemplateHaskell            #-}
module OpEnergy.Offer.Server.V2.OffersAPI.GetById
  ( getById
  , getByIdHandler
  ) where

import           Control.Monad.Trans.Reader(ask)
import           Control.Monad.Trans(lift)
import           Control.Monad.Trans.Except(throwE)
import           Control.Monad.IO.Class(liftIO)
import           Control.Monad.Logger(logError)
import           Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR

import           Database.Persist.Postgresql

import           Data.OpEnergy.Offer.API.V2.OffersAPI(OfferInfo)

import           OpEnergy.Offer.Server.V1.Class(AppM, State(..), profile, runLogging)
import           OpEnergy.Offer.Server.V1.Offer(OfferId, offerInfoFrom)

import           OpEnergy.Error(eitherThrowJSON, runExceptPrefixT, CallstackError, invalidRequest, offerNotFound)

getByIdHandler :: Text -> AppM OfferInfo
getByIdHandler idText =
  let name = "V2.OffersAPI.GetById.getByIdHandler"
  in profile name $ eitherThrowJSON (runLogging . $(logError)) $ getById idText

getById :: Text -> AppM (Either CallstackError OfferInfo)
getById idText =
  let name = "getById"
  in profile name $ runExceptPrefixT name $ do
  key <- case TR.decimal idText of
    Right (n, rest) | T.null rest -> return (toSqlKey n :: OfferId)
    _ -> throwE $ invalidRequest "invalid offer id"
  State{ offerDBPool = pool } <- lift ask
  mOffer <- liftIO $ flip runSqlPersistMPool pool $ get key
  case mOffer of
    Nothing -> throwE offerNotFound
    Just offerVal -> return $! offerInfoFrom idText offerVal
