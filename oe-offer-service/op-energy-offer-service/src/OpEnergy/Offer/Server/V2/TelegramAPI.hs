{-- | Handler wiring for the trade chat.
 -}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE TypeOperators              #-}
module OpEnergy.Offer.Server.V2.TelegramAPI
  ( handlers
  ) where

import           Data.Int (Int64)
import           Servant

import qualified Data.OpEnergy.Account.API.V1.Account as AccountAPI
import           Data.OpEnergy.API.V1.Positive (Positive)
import           Data.OpEnergy.Offer.API.V1.TelegramAPI (TelegramAPI)
import           Data.OpEnergy.Offer.API.V1.TelegramMessageInfo
                 ( TelegramFeed
                 , TelegramMessageID
                 )

import           OpEnergy.Offer.Server.V1.Class (AppM, AppT)

import qualified OpEnergy.Offer.Server.V2.TelegramAPI.BanSender as BanSender
import qualified OpEnergy.Offer.Server.V2.TelegramAPI.DeleteMessage as DeleteMessage
import qualified OpEnergy.Offer.Server.V2.TelegramAPI.GetMessages as GetMessages

-- | see Data.OpEnergy.Offer.API.V1.TelegramAPI for the API definition
handlers :: ServerT TelegramAPI (AppT Handler)
handlers
  = ( GetMessages.getMessagesHandler
      :: Maybe TelegramMessageID -> Maybe (Positive Int) -> AppM TelegramFeed
    )

  :<|> ( DeleteMessage.deleteMessageHandler
         :: TelegramMessageID -> AccountAPI.AccountToken -> AppM NoContent
       )

  :<|> ( BanSender.banSenderHandler
         :: Int64 -> AccountAPI.AccountToken -> AppM NoContent
       )
