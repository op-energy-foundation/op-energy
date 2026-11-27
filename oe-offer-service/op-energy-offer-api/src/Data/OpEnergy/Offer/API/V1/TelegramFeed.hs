{-- | One page of the trade chat, as the website reads it.
 -}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DuplicateRecordFields      #-}
module Data.OpEnergy.Offer.API.V1.TelegramFeed
  ( TelegramFeed(..)
  , defaultTelegramFeed
  ) where

import           Data.Swagger
import           Control.Lens
import           GHC.Generics
import           Data.Typeable              (Typeable)
import           Data.Aeson
import           Data.Word                  (Word64)

import           Data.OpEnergy.Offer.API.V1.TelegramMessageInfo
                 ( TelegramMessageInfo
                 , defaultTelegramMessageInfo
                 )

-- | one page of the trade chat, newest first.
--
-- Paged by the oldest message already shown rather than by a page number: a
-- chat is added to while it is being read, so a page number names a
-- different set of messages every time it is asked for, and a reader
-- scrolling back would be shown a message twice. A boundary, which is
-- itself a message, does not move.
data TelegramFeed = TelegramFeed
  { messages :: [TelegramMessageInfo]
  , limit :: Word64
  , hasMore :: Bool
    -- ^ whether there are older messages still. There is no total: a chat
    -- is read from its newest end and never jumped into by position, so a
    -- count of everything ever said is of no use to a reader and costs a
    -- scan to produce
  }
  deriving (Show, Generic, Typeable)
instance ToJSON   TelegramFeed
instance FromJSON TelegramFeed
instance ToSchema TelegramFeed where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "TelegramFeed schema"
    & mapped.schema.example ?~ toJSON defaultTelegramFeed

defaultTelegramFeed :: TelegramFeed
defaultTelegramFeed = TelegramFeed
  { messages = [ defaultTelegramMessageInfo ]
  , limit = 20
  , hasMore = False
  }
