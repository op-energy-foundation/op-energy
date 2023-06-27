{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE OverloadedStrings          #-}
module OpEnergy.Account.Server.V1.AccountService where

import           Servant (err404, throwError)
import           Control.Monad.Trans.Reader (ask)
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Control.Monad.Logger(logError)
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Short as BS
import           Data.Time.Clock(getCurrentTime)
import           Data.Time.Clock.POSIX(utcTimeToPOSIXSeconds)
import           System.Random( getStdRandom, randomR)
import           Data.Word(Word32)

import qualified Data.Aeson as Aeson
import qualified Web.ClientSession as ClientSession
import           Database.Persist.Postgresql
import           Prometheus(MonadMonitor)
import qualified Prometheus as P


import           Data.OpEnergy.Account.API.V1
import           Data.OpEnergy.Account.API.V1.Hash
import           Data.OpEnergy.Account.API.V1.Account
import           Data.OpEnergy.Account.API.V1.UUID
import           OpEnergy.Account.Server.V1.Config
import           OpEnergy.Account.Server.V1.Class (AppT, AppM, State(..), runLogging)
import           OpEnergy.Account.Server.V1.Metrics(MetricsState(..))


register :: (MonadIO m, MonadMonitor m) => AppT m RegisterResult
register = do
  State{ accountDBPool = pool
       , metrics = MetricsState { accountRegister = accountRegister
                                , accountInsert = accountInsert
                                }
       } <- ask
  P.observeDuration accountRegister $ do
    nowUTC <- liftIO getCurrentTime
    let now = utcTimeToPOSIXSeconds nowUTC
    (uuid, secret, token, tokenCookie, hashedHashedSecret) <- generateRandomUUIDSecret
    -- insert record into cache
    let UUID rawUUID = uuid
        userNameHash = verifyDisplayName $! "user" <> (Text.decodeUtf8 $! BS.take 6 $! BS.fromShort rawUUID)
        person = Person
          { personCreationTime = now
          , personUuid = uuid
          , personLastSeenTime = Nothing
          , personLastUpdated = now
          , personEmail = Nothing
          , personDisplayName = userNameHash
          , personHashedHashedSecret = hashedHashedSecret
          , personLastTokenCookie = Just tokenCookie
          }
    -- insert record into DB
    _ <- liftIO $! P.observeDuration accountInsert $ flip runSqlPersistMPool pool $ insert $! person

    return $! RegisterResult
      { accountSecret = secret
      , accountToken = token
      }
  where
    generateRandomUUIDSecret :: (MonadIO m, MonadMonitor m) => AppT m (UUID Person, AccountSecret, AccountToken, Word32, Hashed (Hashed AccountSecret))
    generateRandomUUIDSecret = do
      State{ config = Config { configSalt = configSalt
                             , configAccountTokenEncryptionPrivateKey = configAccountTokenEncryptionPrivateKey
                             }
           , accountDBPool = pool
           , metrics = MetricsState { accountGenerateUUIDSecret = accountGenerateUUIDSecret
                                    }
           } <- ask
      P.observeDuration accountGenerateUUIDSecret $ do
        uuid <- liftIO generateRandomUUID
        secret <- liftIO $! generateAccountSecret configSalt
        let hashedSecret = hashSBS configSalt unAccountSecret secret
            hashedHashedSecret = hashSBS configSalt unHashed hashedSecret
        mexist <- liftIO $ flip runSqlPersistMPool pool $
          selectFirst (( [ PersonUuid ==. uuid ]
                        ||.
                        [ PersonHashedHashedSecret ==. hashedHashedSecret ]
                      )) []
        case mexist of
          Nothing -> do
            tokenCookie <- liftIO $ getStdRandom (randomR (minBound, maxBound))
            token <- liftIO $ ClientSession.encryptIO configAccountTokenEncryptionPrivateKey $! LBS.toStrict $! Aeson.encode (tokenCookie, hashedSecret)
            return (uuid, secret, verifyAccountToken $! Text.decodeUtf8 token, tokenCookie, hashedHashedSecret)
          Just _ -> generateRandomUUIDSecret
  
login :: AccountSecret -> AppM AccountToken
login secret = do
  State{ config = Config { configSalt = configSalt
                         , configAccountTokenEncryptionPrivateKey = configAccountTokenEncryptionPrivateKey
                         }
       , metrics = MetricsState { accountLogin = accountLogin
                                , accountTokenEncrypt = accountTokenEncrypt
                                }
       } <- ask
  P.observeDuration accountLogin $ do
    tokenCookie <- liftIO $ getStdRandom (randomR (minBound, maxBound))
    let hashedSecret = hashSBS configSalt unAccountSecret secret
    mperson <- mgetPersonByHashedSecretUpdatingTokenCookie hashedSecret tokenCookie
    case mperson of
      Nothing -> throwError err404
      Just _ -> do
        token <- liftIO $ P.observeDuration accountTokenEncrypt $! ClientSession.encryptIO configAccountTokenEncryptionPrivateKey $! LBS.toStrict $! Aeson.encode (tokenCookie, hashedSecret)
        return $! verifyAccountToken $! Text.decodeUtf8 token

mgetPersonByHashedSecret
  :: MonadIO m
  => Hashed AccountSecret
  -> [Update Person] -- list of fields that should be updated alongside with lastSeenTime field. Usually, it will be lastTokenCookie
  -> AppT m (Maybe (Entity Person))
mgetPersonByHashedSecret hashedSecret additionalFieldsUpdate = do
  State{ config = Config { configSalt = configSalt
                         }
       , accountDBPool = pool
       , metrics = MetricsState { accountDBLookup = accountDBLookup
                                , accountMgetPersonByHashedSecret = accountMgetPersonByHashedSecret
                                }
       } <- ask
  liftIO $! P.observeDuration accountMgetPersonByHashedSecret $ do
    nowUTC <- liftIO getCurrentTime
    let now = utcTimeToPOSIXSeconds nowUTC
        hashedHashedSecret = hashSBS configSalt unHashed hashedSecret
    liftIO $! P.observeDuration accountDBLookup $ flip runSqlPersistMPool pool $ do
      mperson <- selectFirst [ PersonHashedHashedSecret ==. hashedHashedSecret ] []
      case mperson of
        Nothing -> return Nothing
        Just (Entity key person) -> do
          update key ([ PersonLastSeenTime =. Just now ] ++ additionalFieldsUpdate)
          return (Just (Entity key person { personLastSeenTime = Just now}))

mgetPersonByHashedSecretTokenCookie
  :: MonadIO m
  => Hashed AccountSecret
  -> Word32
  -> AppT m (Maybe (Entity Person))
mgetPersonByHashedSecretTokenCookie hashedSecret tokenCookie = do
  State{ config = Config { configSalt = configSalt
                         }
       , accountDBPool = pool
       , metrics = MetricsState { accountMgetPersonByHashedSecretTokenCookie = accountMgetPersonByHashedSecretTokenCookie
                                , accountDBLookup = accountDBLookup
                                }
       } <- ask
  liftIO $! P.observeDuration accountMgetPersonByHashedSecretTokenCookie $ do
    nowUTC <- liftIO getCurrentTime
    let now = utcTimeToPOSIXSeconds nowUTC
        hashedHashedSecret = hashSBS configSalt unHashed hashedSecret
    liftIO $! P.observeDuration accountDBLookup $ flip runSqlPersistMPool pool $ do
      mperson <- selectFirst [ PersonHashedHashedSecret ==. hashedHashedSecret
                             , PersonLastTokenCookie ==. Just tokenCookie
                             ] []
      case mperson of
        Nothing -> return Nothing
        Just (Entity key person) -> do
          update key [ PersonLastSeenTime =. Just now ]
          return (Just (Entity key person { personLastSeenTime = Just now}))
  
mgetPersonByHashedSecretUpdatingTokenCookie
  :: ( MonadIO m
     , MonadMonitor m
     )
  => Hashed AccountSecret -- given account secret
  -> Word32 -- new token cookie value
  -> AppT m (Maybe (Entity Person))
mgetPersonByHashedSecretUpdatingTokenCookie hashedSecret tokenCookie = do
  mperson <- mgetPersonByHashedSecret hashedSecret [ PersonLastTokenCookie =. Just tokenCookie]
  case mperson of
    Nothing-> return Nothing
    Just (Entity key person) -> return (Just (Entity key person { personLastTokenCookie = Just tokenCookie} ))

mgetPersonByDisplayName :: MonadIO m => DisplayName-> AppT m (Maybe (Entity Person))
mgetPersonByDisplayName displayName = do
  State{ accountDBPool = pool
       , metrics = MetricsState { accountDBLookup = accountDBLookup
                                }
       } <- ask
  liftIO $! P.observeDuration accountDBLookup $ do
    nowUTC <- liftIO getCurrentTime
    let now = utcTimeToPOSIXSeconds nowUTC
    liftIO $! P.observeDuration accountDBLookup $ flip runSqlPersistMPool pool $ do
      mperson <- selectFirst [ PersonDisplayName ==. displayName ] []
      case mperson of
        Nothing -> return Nothing
        Just (Entity key person) -> do
          update key [ PersonLastSeenTime =. Just now ]
          return (Just (Entity key person { personLastSeenTime = Just now}))

postDisplayName :: PostUserDisplayNameRequest -> AppM ()
postDisplayName (PostUserDisplayNameRequest token newName) = do
  State{ config = Config { configAccountTokenEncryptionPrivateKey = configAccountTokenEncryptionPrivateKey
                         }
       , accountDBPool = pool
       , metrics = MetricsState { accountPostDisplayName = accountPostDisplayName
                                , accountTokenDecrypt = accountTokenDecrypt
                                }
       } <- ask
  P.observeDuration accountPostDisplayName $ do
    mtuple <- P.observeDuration accountTokenDecrypt $! do
      case ClientSession.decrypt configAccountTokenEncryptionPrivateKey $! Text.encodeUtf8 (unAccountToken token) of
        Nothing-> return Nothing
        Just decrypted -> do
          let mtuple :: (Maybe (Word32, Hashed AccountSecret)) = Aeson.decode $! LBS.fromStrict $! decrypted
          case mtuple of
            Nothing-> return Nothing
            Just some -> return (Just some)
    case mtuple of
      Nothing-> do
        let msg = "ERROR: postDisplayName: unable to decrypt account token"
        runLogging $ $(logError) msg
        throwError err404
      Just ( tokenCookie, hashedSecret) -> do
        mperson <- mgetPersonByHashedSecretTokenCookie hashedSecret tokenCookie
        case mperson of
          Nothing -> throwError err404
          Just (Entity key _) -> do
            -- now we need to ensure, that new name do not overlaps with already existing
            mexists <- mgetPersonByDisplayName newName
            case mexists of
              Nothing -> do
                liftIO $ flip runSqlPersistMPool pool $ do
                  nowUTC <- liftIO getCurrentTime
                  let now = utcTimeToPOSIXSeconds nowUTC
                  update key [ PersonDisplayName =. newName
                             , PersonLastUpdated =. now
                             ]
              Just _ -> do
                let msg = "ERROR: postDisplayName: user with given display name already exists"
                runLogging $ $(logError) msg
                throwError err404

-- | performs read from DB in order to set State.currentHeightTip
loadDBState :: AppT IO ()
loadDBState = do
  -- TODO: maybe some cache fill and etc
  return ()
