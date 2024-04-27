{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE DerivingStrategies         #-}
module Data.OpEnergy.Account.API.V1.Account where

import           Data.Swagger
import           Control.Lens
import           Control.Monad (replicateM)
import           System.Random( getStdRandom, randomR)
import           GHC.Generics
import           Data.Typeable              (Typeable)
import           Data.Aeson
import           Data.Text                  (Text)
import qualified Data.Text.Encoding as      TE
import qualified Data.Text as               T
import           Data.Char(isAlphaNum, isSpace)
import           Data.Word
import           Data.Time.Clock.POSIX(POSIXTime)
import           Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as BS (toShort, fromShort)
import qualified Data.ByteString as BS (pack)

import           Database.Persist.TH
import           Database.Persist
import           Database.Persist.Sql
import           Servant.API

import           Data.OpEnergy.Account.API.V1.UUID
import           Data.OpEnergy.Account.API.V1.Hash

newtype AccountSecret = AccountSecret
  { unAccountSecret :: ShortByteString
  }
  deriving (Show, Eq, Generic, Typeable)
instance ToJSON AccountSecret where
  toJSON (AccountSecret s) = toJSON $! TE.decodeUtf8 $! BS.fromShort s
instance FromJSON AccountSecret where
  parseJSON = withText "AccountSecret" $ \v-> return $! AccountSecret $! BS.toShort $! TE.encodeUtf8 v
instance ToSchema AccountSecret where
  declareNamedSchema _ = pure $ NamedSchema (Just "AccountSecret") $ mempty
    & type_ ?~ SwaggerString
instance ToParamSchema AccountSecret where
  toParamSchema _ = mempty
    & type_ ?~ SwaggerString
    & format ?~ "b8ab3013e4ddb35fae6cedc9d84c86fd280157b7a93b984c0b40baf7f21b8f72"


defaultAccountSecret :: AccountSecret
defaultAccountSecret = AccountSecret "a86c139a32e7dac42afe4265a955a0fd9d8c2885e26c7e92d4270b3813faa356"

-- | you can think of this value as a JWT token, but we explicitely require it as part of API calls in order to be able to explicitely describe it with swagger spec
newtype AccountToken = AccountToken
  { unAccountToken:: Text -- base64 encoded encrypted data
  }
  deriving (Eq, Show)
instance ToJSON AccountToken where
  toJSON (AccountToken s) = toJSON s
instance FromJSON AccountToken where
  parseJSON = withText "AccountToken" $ return . verifyAccountToken
instance ToSchema AccountToken where
  declareNamedSchema _ = return $ NamedSchema (Just "AccountToken") $ mempty
    & type_ ?~ SwaggerString
    & example ?~ toJSON defaultAccountToken
instance ToParamSchema AccountToken where
  toParamSchema _ = mempty
    & type_ ?~ SwaggerString
    & format ?~ unAccountToken defaultAccountToken
instance FromHttpApiData AccountToken where
  parseUrlPiece = everifyAccountToken

defaultAccountToken :: AccountToken
defaultAccountToken = verifyAccountToken "h+3b0A7XIfbmjg=="

-- | the goal of this function is to only filter out invalid input. It does not decode/decrypt/analyze payload, just filters out obviously incorrect base64 encoded string
everifyAccountToken :: Text-> Either Text AccountToken
everifyAccountToken raw =
  case () of
    _ | T.any (not . isBase64Char) limitedSize -> Left "everifyAccountToken: some unsupported characters"
    _ -> Prelude.Right (AccountToken limitedSize)
  where
    limitedSize = T.copy $ T.take 2048 raw -- do not allow too big input
    isBase64Char c = isAlphaNum c || c == '=' || c == '+' || c == '/' || c == '_' || c == '-' || c == '*'

-- | the goal of this function is to only filter out invalid input. It does not decode/decrypt/analyze payload, just filters out obviously incorrect base64 encoded string
mverifyAccountToken :: Text-> Maybe AccountToken
mverifyAccountToken v =
  case everifyAccountToken v of
    Left _ -> Nothing
    Prelude.Right r -> Just r

-- | the goal of this function is to only filter out invalid input. It does not decode/decrypt/analyze payload, just filters out obviously incorrect base64 encoded string
verifyAccountToken :: Text-> AccountToken
verifyAccountToken v =
  case everifyAccountToken v of
    Left err -> error $! T.unpack err
    Prelude.Right r -> r

newtype EMailString = EMailString ShortByteString
  deriving (Eq, Show)

instance PersistField EMailString where
  toPersistValue (EMailString s) = toPersistValue $! TE.decodeUtf8 $! BS.fromShort s
  fromPersistValue (PersistText s) = Prelude.Right $! verifyEMailString s
  fromPersistValue _ = Left $ "InputVerification.hs fromPersistValue EMailString, expected Text"
instance PersistFieldSql EMailString where
  sqlType _ = SqlString

everifyEMailString:: Text-> Either Text EMailString
everifyEMailString raw =
  case () of
    _ | not (T.all isDomainChars domain) -> Left "EMailString: domain contains wrong characters"
    _ | T.length (T.filter ( =='.') domain) < 1 -> Left "EMailString: there is no dots in domain"
    _ | not (T.all isNameChars name) -> Left "EMailString: name contains wrong characters"
    _ -> Prelude.Right (EMailString $! BS.toShort $! TE.encodeUtf8 limitedSize)
  where
    limitedSize = T.copy $! T.take 255 raw
    name = T.takeWhile (/= '@') limitedSize
    domain = T.drop (T.length name + 1) limitedSize
    isDomainChars ch = isAlphaNum ch || ch == '.' || ch == '-'
    isNameChars ch = isAlphaNum ch || ch == '.' || ch == '-'

mverifyEMailString:: Text-> Maybe EMailString
mverifyEMailString raw =
  case everifyEMailString raw of
    Left _ -> Nothing
    Prelude.Right ret -> Just ret

verifyEMailString:: Text-> EMailString
verifyEMailString raw =
  case everifyEMailString raw of
    Prelude.Right ret -> ret
    Left some -> error (show some)

newtype DisplayName = DisplayName Text
  deriving (Eq, Show)

instance PersistField DisplayName where
  toPersistValue (DisplayName s) = toPersistValue s
  fromPersistValue (PersistText s) = Prelude.Right $! verifyDisplayName s
  fromPersistValue _ = Left $ "InputVerification.hs fromPersistValue DisplayName , expected Text"
instance PersistFieldSql DisplayName where
  sqlType _ = SqlString
instance ToJSON DisplayName where
  toJSON (DisplayName s) = toJSON s
instance FromJSON DisplayName where
  parseJSON = withText "DisplayName" $ return . verifyDisplayName
instance ToSchema DisplayName where
  declareNamedSchema _ = return $ NamedSchema (Just "DisplayName") $ mempty
    & type_ ?~ SwaggerString
    & example ?~ toJSON defaultDisplayName

defaultDisplayName :: DisplayName
defaultDisplayName = DisplayName "user1234"

everifyDisplayName:: Text-> Either Text DisplayName
everifyDisplayName raw =
  case () of
    _ | not (T.all isDisplayName limitedSize) -> Left "DisplayName: display name contains wrong characters"
    _ -> Prelude.Right (DisplayName limitedSize)
  where
    limitedSize = T.copy $! T.take 255 raw
    isDisplayName ch = isAlphaNum ch || ch == '.' || ch == '-' || isSpace ch

mverifyDisplayName:: Text-> Maybe DisplayName
mverifyDisplayName raw =
  case everifyDisplayName raw of
    Left _ -> Nothing
    Prelude.Right ret -> Just ret

verifyDisplayName:: Text-> DisplayName
verifyDisplayName raw =
  case everifyDisplayName raw of
    Prelude.Right ret -> ret
    Left some -> error (show some)

share [mkPersist sqlSettings, mkMigrate "migrateAccount"] [persistLowerCase|
Person
  -- data
  uuid (UUID Person) -- will be used by other services as foreign key. local relations should use PersonId instead. If you in doubt why not use only Key, then think if you will be able to ensure that Key won't be changed in case of archieving persons, that haven't been seen for a long time.
  hashedSecret (Hashed AccountSecret) -- hash of the secret in order to not to store plain secrets
  loginsCount Word64 -- this field contains an integer value of how many times person had performed login. Default is 0
  email EMailString Maybe -- can be empty (initially)
  displayName DisplayName
  -- metadata
  creationTime POSIXTime
  lastSeenTime POSIXTime -- timestamp of the last seen time. By default the same as creationTime
  lastUpdated POSIXTime -- either CreationTime or last time of the lastest update
  -- constraints
  UniquePersonHashedSecret hashedSecret
  UniqueUUID uuid
  UniqueDisplayName displayName -- it will be confusing if we will allow persons with identical names
  deriving Eq Show Generic
|]

defaultPOSIXTime :: POSIXTime
defaultPOSIXTime = fromIntegral (0::Int)

instance PersistField POSIXTime where
  toPersistValue posix = toPersistValue word
    where
      word :: Word32
      word = floor posix
  fromPersistValue (PersistInt64 i) = Prelude.Right $! fromIntegral i
  fromPersistValue _ = Left $ "InputVerification.hs fromPersistValue POSIXTime, expected Text"
instance PersistFieldSql POSIXTime where
  sqlType _ = SqlInt64

instance PersistField AccountSecret where
  toPersistValue (AccountSecret s) = toPersistValue $! TE.decodeUtf8 $! BS.fromShort s
  fromPersistValue (PersistText s) = Prelude.Right $! verifyAccountSecret s
  fromPersistValue _ = Left $ "InputVerification.hs fromPersistValue AccountSecret, expected Text"
instance PersistFieldSql (AccountSecret) where
  sqlType _ = SqlString


generateAccountSecret :: Text -> IO AccountSecret
generateAccountSecret salt = do
  rndBS <- (replicateM 10 $ getStdRandom (randomR (0::Word8, 255::Word8))) >>= return . BS.pack
  let Hashed hashed = hashSBS salt BS.toShort rndBS
  return $! AccountSecret $! hashed

everifyAccountSecret:: Text-> Either Text AccountSecret
everifyAccountSecret raw =
  case () of
    _ | T.length limitedSize /= 64 -> Left "AccountSecret: wrong size"
    _ | not (T.all isAlphaNum limitedSize ) -> Left "AccountSecret: should be alpha num"
    _ -> Prelude.Right (AccountSecret $! BS.toShort $! TE.encodeUtf8 limitedSize)
  where
    limitedSize = T.copy $! T.take 64 raw

mverifyAccountSecret:: Text-> Maybe AccountSecret
mverifyAccountSecret raw =
  case everifyAccountSecret raw of
    Left _ -> Nothing
    Prelude.Right ret -> Just ret

verifyAccountSecret:: Text-> AccountSecret
verifyAccountSecret raw =
  case everifyAccountSecret raw of
    Prelude.Right ret -> ret
    Left some -> error (show some)
