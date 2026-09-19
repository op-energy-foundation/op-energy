{-- | GET /api/v2/offer/:id
 -}
{-# LANGUAGE TemplateHaskell            #-}
module OpEnergy.Offer.Server.V2.OffersAPI.GetById
  ( getById
  , getByIdHandler
  ) where

import           Control.Monad.Trans.Reader(ask)
import           Control.Monad.Trans(lift)
import           Control.Monad.IO.Class(liftIO)
import           Control.Monad.Logger(logError)
import           Data.Text(Text)

import           Database.Persist.Postgresql

import           Data.OpEnergy.Offer.API.V1.OfferID(OfferID(..))
import           Data.OpEnergy.Offer.API.V1.OfferInfo(OfferInfo)

import           OpEnergy.Offer.Server.V1.Class(AppM, State(..), profile, runLogging)
import           OpEnergy.Offer.Server.V1.Offer
                   ( offerInfoFrom
                   , offerKeyFromIDText
                   )

import           OpEnergy.Error
                   ( eitherThrowJSON, runExceptPrefixT, exceptTMaybeT
                   , CallstackError, invalidRequest, offerNotFound
                   )

getByIdHandler :: OfferID -> AppM OfferInfo
getByIdHandler (OfferID idText) =
  let name = "V2.OffersAPI.GetById.getByIdHandler"
  in profile name $ eitherThrowJSON (runLogging . $(logError)) $ getById idText

getById :: Text -> AppM (Either CallstackError OfferInfo)
getById idText =
  let name = "V2.OffersAPI.GetById.getById"
  in profile name $ runExceptPrefixT name $ do
  key <- exceptTMaybeT (invalidRequest "invalid offer id")
    $ return (offerKeyFromIDText idText)
  State{ offerDBPool = pool } <- lift ask
  offerVal <- exceptTMaybeT offerNotFound
    $! liftIO $ flip runSqlPersistMPool pool $ get key
  return $! offerInfoFrom idText offerVal
