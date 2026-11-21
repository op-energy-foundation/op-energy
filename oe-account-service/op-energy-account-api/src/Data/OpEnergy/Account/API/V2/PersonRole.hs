{-- | What an account is allowed to do beyond acting for itself.
 -
 - Every account is a 'User': it posts offers, accepts them and talks in the
 - chat of a contract it is part of. A 'Moderator' may additionally read any
 - contract's chat, remove a message from it and ban an account. An 'Admin'
 - is a moderator, which is expected to grow further powers later; it grants
 - nothing beyond moderator today.
 -
 - The role is stored on the person and reported by both '/me' and
 - '/whoami', so the offer service learns it from the whoami answer it
 - already asks for on every authenticated request, with no second call.
 -}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE OverloadedStrings          #-}
module Data.OpEnergy.Account.API.V2.PersonRole
  ( PersonRole(..)
  , defaultPersonRole
  , personRoleToText
  , personRoleFromText
  , isModerator
  ) where

import           Data.Aeson
import           Data.Text                  (Text)
import           Data.Typeable              (Typeable)
import           GHC.Generics
import           Control.Lens               ((&), (?~))
import           Data.Swagger
import           Servant.API                (FromHttpApiData(..), ToHttpApiData(..))

-- | the closed set of roles an account can hold.
--
-- Ordered by what they may do, least first, so 'Ord' answers "at least a
-- moderator?" without a case of its own.
data PersonRole
  = User -- ^ acts for itself only
  | Moderator -- ^ may also read any chat, delete a message and ban an account
  | Admin -- ^ a moderator today; reserved for powers added later
  deriving (Show, Eq, Ord, Generic, Typeable, Enum, Bounded)

-- | lowercase serialisation for JSON and query params, so the stored value
-- stays readable in psql
personRoleToText :: PersonRole -> Text
personRoleToText User = "user"
personRoleToText Moderator = "moderator"
personRoleToText Admin = "admin"

personRoleFromText :: Text -> Either Text PersonRole
personRoleFromText "user" = Right User
personRoleFromText "moderator" = Right Moderator
personRoleFromText "admin" = Right Admin
personRoleFromText other =
  Left $ "PersonRole: unknown role: " <> other

defaultPersonRole :: PersonRole
defaultPersonRole = User

-- | whether this role carries the moderator powers. Both 'Moderator' and
-- 'Admin' do, so every guard asks this rather than matching on a
-- constructor and forgetting the other one.
--
-- Example:
--
-- > isModerator Admin -- True
isModerator :: PersonRole -> Bool
isModerator role = role >= Moderator

instance ToJSON PersonRole where
  toJSON = toJSON . personRoleToText
instance FromJSON PersonRole where
  parseJSON = withText "PersonRole"
    $ either (fail . show) pure . personRoleFromText
instance ToSchema PersonRole where
  declareNamedSchema _ = pure $ NamedSchema (Just "PersonRole") $ mempty
    & type_ ?~ SwaggerString
    & enum_ ?~ map (toJSON . personRoleToText) [minBound .. maxBound]
    & example ?~ toJSON defaultPersonRole
instance ToParamSchema PersonRole where
  toParamSchema _ = mempty
    & type_ ?~ SwaggerString
    & enum_ ?~ map (toJSON . personRoleToText) [minBound .. maxBound]
instance FromHttpApiData PersonRole where
  parseQueryParam = personRoleFromText
instance ToHttpApiData PersonRole where
  toQueryParam = personRoleToText
