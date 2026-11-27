{-- | The one time code, which connects an account's telegram.
 -}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DuplicateRecordFields      #-}
module Data.OpEnergy.Offer.API.V1.TelegramLinkStart
  ( TelegramLinkStart(..)
  , defaultTelegramLinkStart
  ) where

import           Data.Swagger
import           Control.Lens
import           GHC.Generics
import           Data.Typeable              (Typeable)
import           Data.Aeson
import           Data.Text                  (Text)
import           Data.Word                  (Word64)

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
