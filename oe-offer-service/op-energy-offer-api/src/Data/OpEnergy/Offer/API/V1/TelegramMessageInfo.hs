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
  ( TelegramMessageInfo(..)
  , defaultTelegramMessageInfo
  ) where

import           Data.Swagger
import           Control.Lens
import           GHC.Generics
import           Data.Typeable              (Typeable)
import           Data.Aeson
import           Data.Int                   (Int64)
import           Data.Text                  (Text)
import           Data.Time.Clock            (UTCTime)

import           Data.OpEnergy.Offer.API.V1.TelegramMessageID
                 ( TelegramMessageID
                 , defaultTelegramMessageID
                 )

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
