{-- | Typed wrapper for the ids telegram gives the messages of a group.
 -
 - Extracted to its own module, as 'OfferID' is, so the feed and the message
 - can each name one without importing the other.
 -}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE OverloadedStrings          #-}
module Data.OpEnergy.Offer.API.V1.TelegramMessageID
  ( TelegramMessageID(..)
  , everifyTelegramMessageID
  , telegramMessageIDNumber
  , defaultTelegramMessageID
  ) where

import           Data.Swagger
import           Control.Lens               ((&), (?~))
import           GHC.Generics
import           Data.Typeable              (Typeable)
import           Data.Aeson
import           Data.Int                   (Int64)
import           Data.Text                  (Text)
import qualified Data.Text as Text
import           Servant.API                (FromHttpApiData(..), ToHttpApiData(..))
import           Text.Read                  (readMaybe)

-- | telegram's own id for a message, which is what a moderator's delete
-- names. Held as text, as 'OfferID' and every other id of this API is, so a
-- client never has to care what a given service numbers things with.
newtype TelegramMessageID = TelegramMessageID { unTelegramMessageID :: Text }
  deriving (Show, Eq, Generic, Typeable)

-- | the number telegram would recognise, or the reason it is not one.
--
-- The text of a 'TelegramMessageID' has been verified on the way in, so a
-- handler, which has one, can take the number out of it without a second
-- opinion about what to do when it is not a number.
--
-- Example:
--
-- > everifyTelegramMessageID "4211" -- Right (TelegramMessageID "4211")
-- > everifyTelegramMessageID "4211x" -- Left "..."
-- > everifyTelegramMessageID "0" -- Left "..."
-- > everifyTelegramMessageID "-1" -- Left "..."
everifyTelegramMessageID :: Text -> Either Text TelegramMessageID
everifyTelegramMessageID raw = case readMaybe (Text.unpack raw) :: Maybe Int64 of
  Nothing -> Left notANumber
  -- telegram numbers from one upward, so nothing it ever sent is named by
  -- zero or by a negative. Refused here rather than carried: an id, which
  -- names no message, reaches telegram and comes back as a refusal, which
  -- reads as a message too old to delete -- a different thing entirely
  Just n | n <= 0 -> Left notANumber
  Just _ -> Right $! TelegramMessageID raw
  where
    notANumber =
      "TelegramMessageID: telegram numbers its messages from one, so this\
      \ is not one"

-- | the id as the number telegram speaks in, where it is one.
--
-- Every id, which arrived over the API, has been through
-- 'everifyTelegramMessageID', so this answers 'Just' for all of them. It
-- is a 'Maybe' rather than a plain 'Int64' because the constructor is
-- exported -- the mirror builds ids from telegram's own numbers -- and a
-- fallback number here would quietly name whatever message it happened to
-- pick.
telegramMessageIDNumber :: TelegramMessageID -> Maybe Int64
telegramMessageIDNumber (TelegramMessageID raw) =
  readMaybe (Text.unpack raw)

instance ToJSON TelegramMessageID where
  toJSON (TelegramMessageID t) = toJSON t
-- deserialisation verifies, so an id, which no message could have, is
-- refused where it arrives rather than where it is used
instance FromJSON TelegramMessageID where
  parseJSON = withText "TelegramMessageID"
    $ either (fail . Text.unpack) pure . everifyTelegramMessageID
instance ToSchema TelegramMessageID where
  declareNamedSchema _ = pure $ NamedSchema (Just "TelegramMessageID") $ mempty
    & type_ ?~ SwaggerString
    & example ?~ toJSON defaultTelegramMessageID
instance ToParamSchema TelegramMessageID where
  toParamSchema _ = mempty
    & type_ ?~ SwaggerString
instance FromHttpApiData TelegramMessageID where
  parseQueryParam = everifyTelegramMessageID
  parseUrlPiece = everifyTelegramMessageID
instance ToHttpApiData TelegramMessageID where
  toQueryParam (TelegramMessageID t) = t

defaultTelegramMessageID :: TelegramMessageID
defaultTelegramMessageID = TelegramMessageID "4211"
