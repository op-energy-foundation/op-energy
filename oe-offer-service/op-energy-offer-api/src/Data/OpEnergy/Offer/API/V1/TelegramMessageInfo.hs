{-- | One message of the trade chat, as the website is given it.
 -
 - The chat is held in a telegram group and this service keeps a copy of it,
 - so these are the copied messages rather than anything said here.
 -
 - A sender is shown by their telegram name, which every message carries. An
 - account, which has connected its telegram, is additionally shown by its
 - op energy name, which is why both are here and why the second one is
 - optional: most senders will not have connected anything.
 -}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DuplicateRecordFields      #-}
module Data.OpEnergy.Offer.API.V1.TelegramMessageInfo
  ( TelegramMessageID(..)
  , defaultTelegramMessageID
  , TelegramMessageInfo(..)
  , defaultTelegramMessageInfo
  , TelegramFeed(..)
  , defaultTelegramFeed
  , TelegramLinkStart(..)
  , defaultTelegramLinkStart
  ) where

import           Data.Swagger
import           Control.Lens
import           GHC.Generics
import           Data.Typeable              (Typeable)
import           Data.Aeson
import           Data.Int                   (Int64)
import           Data.Text                  (Text)
import           Data.Time.Clock            (UTCTime)
import           Data.Word                  (Word64)
import           Servant.API                (FromHttpApiData(..), ToHttpApiData(..))

-- | telegram's own id for a message, which is what a moderator's delete
-- names. Carried as text, as every other id in this API is, so a client
-- never has to care that telegram's are numbers.
newtype TelegramMessageID = TelegramMessageID { unTelegramMessageID :: Text }
  deriving (Show, Eq, Generic, Typeable)
instance ToJSON TelegramMessageID where
  toJSON (TelegramMessageID t) = toJSON t
instance FromJSON TelegramMessageID where
  parseJSON = withText "TelegramMessageID" $ pure . TelegramMessageID
instance ToSchema TelegramMessageID where
  declareNamedSchema _ = pure $ NamedSchema (Just "TelegramMessageID") $ mempty
    & type_ ?~ SwaggerString
    & example ?~ toJSON defaultTelegramMessageID
instance ToParamSchema TelegramMessageID where
  toParamSchema _ = mempty
    & type_ ?~ SwaggerString
instance FromHttpApiData TelegramMessageID where
  parseQueryParam = Right . TelegramMessageID
  parseUrlPiece = Right . TelegramMessageID
instance ToHttpApiData TelegramMessageID where
  toQueryParam (TelegramMessageID t) = t

defaultTelegramMessageID :: TelegramMessageID
defaultTelegramMessageID = TelegramMessageID "4211"

-- | one message of the trade chat
data TelegramMessageInfo = TelegramMessageInfo
  { messageId :: TelegramMessageID
  , senderName :: Text
    -- ^ what the sender is called in telegram: their first name, which
    -- telegram always has. A message sent by an anonymous administrator is
    -- shown under the group's title instead
  , senderDisplayName :: Maybe Text
    -- ^ their op energy name, where they have connected their telegram to
    -- an account. Absent for everyone else, which is most senders, so a
    -- feed row has to read properly with only 'senderName'
  , senderId :: Maybe Int64
    -- ^ telegram's id for the sender, which is what banning them takes.
    -- Absent for an anonymous administrator, who cannot be banned
  , body :: Text
  , sentAt :: UTCTime
  , editedAt :: Maybe UTCTime
  }
  deriving (Show, Eq, Generic, Typeable)
instance ToJSON   TelegramMessageInfo
instance FromJSON TelegramMessageInfo
instance ToSchema TelegramMessageInfo where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "TelegramMessageInfo schema"
    & mapped.schema.example ?~ toJSON defaultTelegramMessageInfo

defaultTelegramMessageInfo :: TelegramMessageInfo
defaultTelegramMessageInfo = TelegramMessageInfo
  { messageId = defaultTelegramMessageID
  , senderName = "Naimish"
  , senderDisplayName = Just "blur_inject_439"
  , senderId = Just 775566
  , body = "good luck"
  , sentAt = read "2026-11-20 12:00:00 UTC"
  , editedAt = Nothing
  }

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

-- | the one time code, which connects an account's telegram.
--
-- The website puts it in a link to the bot; pressing Start there sends it
-- back from the telegram account, which pressed it, and that is what proves
-- the two are the same person.
data TelegramLinkStart = TelegramLinkStart
  { linkCode :: Text
  , expiresInSeconds :: Word64
    -- ^ how long the code is good for. Short, because anybody, who got hold
    -- of it before it was spent, could connect their own telegram to this
    -- account
  }
  deriving (Show, Generic, Typeable)
instance ToJSON   TelegramLinkStart
instance FromJSON TelegramLinkStart
instance ToSchema TelegramLinkStart where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "TelegramLinkStart schema"
    & mapped.schema.example ?~ toJSON defaultTelegramLinkStart

defaultTelegramLinkStart :: TelegramLinkStart
defaultTelegramLinkStart = TelegramLinkStart
  { linkCode = "qx7m2kpb9rtv"
  , expiresInSeconds = 600
  }
