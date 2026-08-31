{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Data.OpEnergy.Account.API.V1.Password
  ( Password(..)
  , HashedPassword(..)
  , everifyPassword
  ) where

import           Data.Text                  (Text)
import qualified Data.Text as T
import           Data.Typeable              (Typeable)
import           GHC.Generics
import           Data.Aeson
import           Data.Swagger

-- | a plaintext password as it arrives from a client. Validated on
-- deserialization: 8–100 characters.
newtype Password = Password { unPassword :: Text }
  deriving (Show, Eq, Generic, Typeable)

-- | validates password length (8–100 characters)
everifyPassword :: Text -> Either Text Password
everifyPassword raw
  | T.length raw < 8   = Left "password must be at least 8 characters"
  | T.length raw > 100 = Left "password must be at most 100 characters"
  | otherwise           = Right (Password raw)

instance FromJSON Password where
  parseJSON = withText "Password" $ \t ->
    case everifyPassword t of
      Left err -> fail (T.unpack err)
      Right p  -> return p

instance ToJSON Password where
  toJSON (Password t) = toJSON t

instance ToSchema Password where
  declareNamedSchema _ = return $ NamedSchema (Just "Password") mempty

-- | a bcrypt digest of the password, stored in the database.
-- Hashed rather than encrypted: a password only ever needs
-- verifying, never recovering.
newtype HashedPassword = HashedPassword { unHashedPassword :: Text }
  deriving (Show, Eq, Generic, Typeable)

instance ToJSON HashedPassword where
  toJSON (HashedPassword t) = toJSON t

instance FromJSON HashedPassword where
  parseJSON = withText "HashedPassword" $ return . HashedPassword

instance ToSchema HashedPassword where
  declareNamedSchema _ = return $ NamedSchema (Just "HashedPassword") mempty
