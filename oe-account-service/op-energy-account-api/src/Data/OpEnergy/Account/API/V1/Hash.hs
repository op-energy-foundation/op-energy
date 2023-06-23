{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances          #-}
module Data.OpEnergy.Account.API.V1.Hash where

import           Data.Text                  (Text)
import qualified Data.Text.Encoding as      T
import qualified Data.Text as               T
import           Data.ByteString.Short( ShortByteString)
import qualified Data.ByteString.Short as BS (toShort, fromShort)
import qualified Data.ByteString as BS (append)
import           GHC.Generics
import           Data.Typeable              (Typeable)
import           Data.Aeson
import           Data.Swagger
import           Control.Lens
import           Servant.API
import           Data.Char (isAlphaNum)
import           Crypto.Hash.SHA256
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Text.Encoding as TE

import           Database.Persist
import           Database.Persist.Sql

newtype Hashed a = Hashed
  { unHashed :: ShortByteString -- ShortByteString here is just because
  }
               -- regular ByteString is not compatible with Compact regions
  deriving (Show, Generic, Typeable, Eq, Ord)
instance ToJSON (Hashed a) where
  toJSON (Hashed s) = toJSON $! TE.decodeUtf8 $! BS.fromShort s
instance FromJSON (Hashed a) where
  parseJSON = withText "Hashed" $ \v-> return $! verifyHash v
instance ToParamSchema (Hashed a) where
  toParamSchema _ = mempty
    & type_ ?~ SwaggerString
    & format ?~ "b8ab3013e4adb35fae6cbdc9d84c86cd280157b7a93b984c0b40baf7f21b8f72"
instance ToHttpApiData (Hashed a)  where
  toUrlPiece (Hashed t) = toUrlPiece $! TE.decodeUtf8 $! BS.fromShort t
  toQueryParam (Hashed t) = toQueryParam $! TE.decodeUtf8 $! BS.fromShort t
instance ToSchema (Hashed a) where
  declareNamedSchema _ = pure $ NamedSchema (Just "Hash") $ mempty
    & type_ ?~ SwaggerString
instance FromHttpApiData (Hashed a) where
  parseUrlPiece t = Right (verifyHash t)
  parseQueryParam t = Right (verifyHash t)

instance PersistField (Hashed a) where
  toPersistValue (Hashed s) = toPersistValue $! TE.decodeUtf8 $! BS.fromShort s
  fromPersistValue (PersistText s) = Right $! verifyHash s -- TODO
  fromPersistValue _ = Left $ "InputVerification.hs fromPersistValue Hashed , expected Text"
instance PersistFieldSql (Hashed a) where
  sqlType _ = SqlString


defaultHash :: Hashed a
defaultHash = Hashed "b8ab3013e4adb35fae6cbdc9d84c86cd280157b7a93b984c0b40baf7f21b8f72"

everifyHash:: Text-> Either Text (Hashed a)
everifyHash raw =
  case () of
    _ | T.length limitedSize /= 64 -> Left "Hashed: wrong size"
    _ | not (T.all isAlphaNum limitedSize ) -> Left "Hashed: should be alpha num"
    _ -> Right (Hashed $! BS.toShort $! TE.encodeUtf8 limitedSize)
  where
    limitedSize = T.copy $! T.take 64 raw

mverifyHash:: Text-> Maybe (Hashed a)
mverifyHash raw =
  case everifyHash raw of
    Left _ -> Nothing
    Right ret -> Just ret

verifyHash:: Text-> Hashed a
verifyHash raw =
  case everifyHash raw of
    Right ret -> ret
    Left some -> error (show some)

hashSBS :: Text-> (a -> ShortByteString) -> a -> Hashed a
hashSBS salt _ _ | T.length salt < 1 = error "hashSBS: salt is empty"
hashSBS salt toSBS v =
  let saltBS = T.encodeUtf8 salt
      base16 = Base16.encode $! hash $! (BS.fromShort $! (toSBS v)) `BS.append` saltBS
  in Hashed $! BS.toShort $! base16

hashText :: Text-> (a -> Text) -> a -> Hashed a
hashText salt _ _ | T.length salt < 1 = error "hashText: salt is empty"
hashText salt toText v = hashSBS salt (BS.toShort . T.encodeUtf8 . toText) v
  
