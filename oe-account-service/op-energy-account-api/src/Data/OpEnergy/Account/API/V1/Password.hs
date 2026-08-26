{-- | This module defines the password credential types: the plaintext
 - 'Password' as it arrives from a client, and 'HashedPassword' as it is
 - stored in the DB.
 -
 - Unlike 'Data.OpEnergy.Account.API.V1.Hash.Hashed', which holds a 64-char
 - hex SHA256 digest, a bcrypt digest is ~60 characters and contains '$' and
 - '/' separators, so it needs its own type rather than reusing 'Hashed'.
 -
 - Passwords are hashed (never encrypted): the service only ever needs to
 - verify a candidate against the stored digest, never to recover the
 - original. This is deliberately different from 'AccountSecret', which is
 - stored recoverably because the UI has to display it back to the user.
 -}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE OverloadedStrings          #-}
module Data.OpEnergy.Account.API.V1.Password
  ( Password(..)
  , HashedPassword(..)
  , everifyPassword
  , verifyPassword
  , minPasswordLength
  , maxPasswordLength
  )where

import           Data.Aeson
import           Data.Text                  (Text)
import qualified Data.Text as T
import           Data.Typeable              (Typeable)
import           GHC.Generics
import           Control.Lens               ((&), (?~))
import           Data.Swagger

import           Database.Persist
import           Database.Persist.Sql

-- | plaintext password, as received from a client. Only ever used in
-- transit: it is hashed before it reaches the DB and is never stored nor
-- logged in this form.
newtype Password = Password
  { unPassword :: Text
  }
  deriving (Show, Eq, Generic, Typeable)
instance ToJSON Password where
  toJSON (Password s) = toJSON s
instance FromJSON Password where
  parseJSON = withText "Password" $ \v-> return $! Password v
instance ToSchema Password where
  declareNamedSchema _ = pure $ NamedSchema (Just "Password") $ mempty
    & type_ ?~ SwaggerString

-- | bcrypt digest of a 'Password', as stored in the DB. Carries the bcrypt
-- cost parameter and salt inside itself, so no separate salt column is
-- needed.
newtype HashedPassword = HashedPassword
  { unHashedPassword :: Text
  }
  deriving (Show, Eq, Generic, Typeable)
instance ToJSON HashedPassword where
  toJSON (HashedPassword s) = toJSON s
instance FromJSON HashedPassword where
  parseJSON = withText "HashedPassword" $ \v-> return $! HashedPassword v
instance ToSchema HashedPassword where
  declareNamedSchema _ = pure $ NamedSchema (Just "HashedPassword") $ mempty
    & type_ ?~ SwaggerString

instance PersistField HashedPassword where
  toPersistValue (HashedPassword s) = toPersistValue s
  fromPersistValue (PersistText s) = Right $! HashedPassword s
  fromPersistValue _ =
    Left "Password.hs fromPersistValue HashedPassword, expected Text"
instance PersistFieldSql HashedPassword where
  sqlType _ = SqlString

-- | the shortest password we accept. Short enough to stay usable for a
-- sandbox account, long enough that a bcrypt digest is worth having.
minPasswordLength :: Int
minPasswordLength = 8

-- | bcrypt only considers the first 72 bytes of its input, so accepting
-- more than that would silently ignore the tail
maxPasswordLength :: Int
maxPasswordLength = 72

-- | verifies that a client-supplied password is of an acceptable shape.
-- Returns the reason as @Left@ rather than throwing, so callers can decide
-- how to report it.
everifyPassword :: Text-> Either Text Password
everifyPassword raw =
  case () of
    _ | T.length raw < minPasswordLength ->
        Left "Password: too short"
    _ | T.length raw > maxPasswordLength ->
        Left "Password: too long"
    _ -> Right (Password $! T.copy raw)

-- | partial version of 'everifyPassword'. Only for use on values which are
-- already known to be well-formed, as it calls 'error' otherwise.
verifyPassword :: Text-> Password
verifyPassword raw =
  case everifyPassword raw of
    Right ret -> ret
    Left some -> error (show some)
