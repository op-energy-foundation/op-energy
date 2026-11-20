{-- |
 - This module is the top module of the Offer service's V1 API.
 - Server module paths remain at V2 for historical reasons; only the
 - API types moved to V1.
 -}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module OpEnergy.Offer.Server.V2
  ( offerServer
  )where

import           Servant

import           OpEnergy.Offer.Server.V1.Class ( AppT)

import           Data.OpEnergy.Offer.API.V1 (OfferV1API)
import qualified OpEnergy.Offer.Server.V2.PostOfferAPI as PostOfferAPI
import qualified OpEnergy.Offer.Server.V2.OffersAPI as OffersAPI
import qualified OpEnergy.Offer.Server.V2.CancelAPI as CancelAPI
import qualified OpEnergy.Offer.Server.V2.AcceptAPI as AcceptAPI
import qualified OpEnergy.Offer.Server.V2.TelegramAPI as TelegramAPI
import           Data.OpEnergy.Offer.API.V1.TelegramAPI (TelegramAPI)
import           Data.OpEnergy.Offer.API.V1.OffersAPI (OffersAPI)

-- | V1 offer server wiring. OffersAPI is one combined sub-API:
-- post :<|> cancel :<|> accept :<|> mine :<|> list :<|> contracts
-- :<|> getById :<|> getDetails
offerServer :: ServerT OfferV1API (AppT Handler)
offerServer
  = ( offersServer :: ServerT OffersAPI (AppT Handler) )
  :<|> ( TelegramAPI.handlers :: ServerT TelegramAPI (AppT Handler) )

-- | the offer endpoints, grouped so the chat is a sub-API beside them
-- rather than another route of theirs
offersServer :: ServerT OffersAPI (AppT Handler)
offersServer
  =    PostOfferAPI.postHandler
  :<|> CancelAPI.cancelHandler
  :<|> AcceptAPI.acceptHandler
  :<|> OffersAPI.getMineHandler
  :<|> OffersAPI.getListHandler
  :<|> OffersAPI.getContractsHandler
  :<|> OffersAPI.getByIdHandler
  :<|> OffersAPI.getDetailsHandler
