{-- | The trade chat: a read only feed of the telegram group it is held in,
 - and what a moderator may do to it.
 -
 - Nothing here posts a message. Talking happens in telegram, which is what
 - the Join link in the website is for; this service only keeps a copy and
 - serves it.
 -}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
module Data.OpEnergy.Offer.API.V1.TelegramAPI
  ( TelegramAPI
  ) where

import           Data.Int (Int64)
import           Servant.API

import           Data.OpEnergy.API.V1.Positive (Positive)
import           Data.OpEnergy.Account.API.V1.Account (AccountToken)
import           Data.OpEnergy.Offer.API.V1.TelegramMessageInfo
                 ( TelegramFeed
                 , TelegramMessageID
                 )

-- | the trade chat's endpoints
type TelegramAPI
  = "messages"
    :> QueryParam'
       '[ Optional
        , Strict
        , Description "return the messages older than this one, which is \
                      \how the feed is scrolled back. Omitted asks for the \
                      \newest"
        ]
       "before"
       TelegramMessageID
    :> QueryParam'
       '[ Optional
        , Strict
        , Description "how many to return, default 20, capped at 100"
        ]
       "limit"
       (Positive Int)
    :> Description "The trade chat, newest first. Paged by the oldest \
                   \message already shown rather than by a page number: a \
                   \chat is added to while it is read, so a page number \
                   \names a different set of messages each time it is \
                   \asked for. Empty where this deployment mirrors no \
                   \telegram group."
    :> Get '[JSON] TelegramFeed

  :<|> "messages"
    :> Capture' '[ Description "the message to remove" ]
       "messageId" TelegramMessageID
    :> "delete"
    :> Header'
       '[ Required
        , Strict
        , Description "Account token of a moderator"
        ]
       "Authorization"
       AccountToken
    :> Description "Removes a message from the telegram group itself, not \
                   \only from this feed. Refused with 409 where telegram \
                   \will no longer delete it, which it will not once it is \
                   \about two days old: the message is still in the group, \
                   \and the feed goes on showing it for that reason. \
                   \Refused with 403 unless the caller is a moderator."
    :> Post '[JSON] NoContent

  :<|> "senders"
    :> Capture' '[ Description "telegram's id for the sender to ban" ]
       "telegramUserId" Int64
    :> "ban"
    :> Header'
       '[ Required
        , Strict
        , Description "Account token of a moderator"
        ]
       "Authorization"
       AccountToken
    :> Description "Bans an account from the telegram group. This is a ban \
                   \in telegram and not on this platform: the two are \
                   \separate, and banning somebody here does not remove \
                   \them from the group. Refused with 403 unless the \
                   \caller is a moderator."
    :> Post '[JSON] NoContent
