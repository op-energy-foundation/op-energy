{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.OpEnergy.Account.API.V1.UUID where

import           Data.ByteString.Short( ShortByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as BS (fromShort, toShort)
import           GHC.Generics
import           Data.Typeable              (Typeable)
import           Data.Char (isAlphaNum)
import           Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Word
import           Control.Monad (replicateM)
import           System.Random(getStdRandom, randomR)

import qualified Data.ByteString.Base16 as Base16
import           Crypto.Hash.SHA256
import           Data.Aeson
import           Database.Persist
import           Database.Persist.Sql
import           Data.Swagger
import           Control.Lens
import           Data.Proxy(Proxy(..))
import           Servant.API

newtype UUID a = UUID { unUUID:: ShortByteString}
  deriving (Eq, Show, Generic, Typeable)

instance ToJSON (UUID a) where
  toJSON (UUID s) = toJSON $! TE.decodeUtf8 $! BS.fromShort s
instance FromJSON (UUID a) where
  parseJSON = withText "UUID" $ pure . verifyUUID
instance Show a => ToSchema (UUID a) where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ (T.unlines
      [ "UUID is an unique id of some resource"
      ])
    & mapped.schema.type_ ?~ SwaggerString
    & mapped.schema.properties .~ []
    & mapped.schema.required .~ []
    & mapped.schema.example ?~ toJSON defaultUUID
instance ToSchema ShortByteString where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)
instance ToParamSchema (UUID a) where
  toParamSchema _ = mempty
    & type_ ?~ SwaggerString
    & format ?~ (TE.decodeUtf8 $ BS.fromShort $ unUUID defaultUUID)
instance FromHttpApiData (UUID a) where
  parseUrlPiece t = Right (verifyUUID t)
  parseQueryParam t = Right (verifyUUID t)


generateRandomUUID :: IO (UUID a)
generateRandomUUID = do
  rndBS <- (replicateM 10 $ getStdRandom (randomR (0::Word8, 255::Word8))) >>= return . BS.pack
  let base16 = Base16.encode $! hash rndBS
  return $! UUID $! BS.toShort $! base16

instance PersistField (UUID a) where
  toPersistValue (UUID s) = toPersistValue $! TE.decodeUtf8 $! BS.fromShort s
  fromPersistValue (PersistText s) = Prelude.Right $! verifyUUID s
  fromPersistValue _ = Left $ "InputVerification.hs fromPersistValue Natural, expected Text"
instance PersistFieldSql (UUID a) where
  sqlType _ = SqlString

everifyUUID:: Text-> Either Text (UUID a)
everifyUUID raw =
  case () of
    _ | T.length limitedSize /= 64 -> Left "UUID: wrong size"
    _ | not (T.all isAlphaNum limitedSize ) -> Left "UUID: should be alpha num"
    _ -> Right (UUID $! BS.toShort $! TE.encodeUtf8 limitedSize)
  where
    limitedSize = T.copy $! T.take 64 raw

mverifyUUID:: Text-> Maybe (UUID a)
mverifyUUID raw =
  case everifyUUID raw of
    Left _ -> Nothing
    Right ret -> Just ret

verifyUUID:: Text-> UUID a
verifyUUID raw =
  case everifyUUID raw of
    Right ret -> ret
    Left some -> error (show some)

defaultUUID :: UUID a
defaultUUID = UUID "b8ab3013e4adb35fae6cbdc9d84c86cd280157b7a93b984c0b40baf7f21b8f72"
