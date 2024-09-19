{-- | This module implements Account service in terms of OpEnergy.Account.API.V1.AccountV1API API, which you may
 - be insteresting to check for API description.
 -
 - Implementation details are defined in op-energy/README.md
 -
 - TODO: we may consider:
 - 1. using some random value of say [ 0; 10000] as default value for loginsCount instead of 0;
 - 2. increasing loginsCount not by 1, but by random value [1;10000].
 - This way we can increase an entropy in case of worries of lacking of randomness in token. This will reduce the time
 - to overflow the loginsCount accumulator, but it is expected to happen at some point anyway...
 -}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE OverloadedStrings          #-}
module OpEnergy.Account.Server.V1.AccountService
  ( register
  , login
  , postDisplayName
  , loadDBState
  , mgetPersonByAccountToken -- supposed that another services will use this function to authenticate user
  , mgetPersonByHashedSecret
  ) where

import           Servant (err400)
import           Control.Monad.Trans.Reader (ask)
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Control.Monad.Logger(logError)
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Short as BS
import           Data.Time.Clock(getCurrentTime)
import           Data.Time.Clock.POSIX(utcTimeToPOSIXSeconds)
import           Data.Word(Word64)

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
import           Data.OpEnergy.API.V1.Error


-- | see OpEnergy.Account.API.V1.AccountV1API for reference of 'register' API call
-- Current implementation is 2 * O(ln n)
register :: (MonadIO m, MonadMonitor m) => AppT m RegisterResult
register = do
  State{ config = Config { configSalt = configSalt
                         , configAccountTokenEncryptionPrivateKey = configAccountTokenEncryptionPrivateKey
                         }
       , accountDBPool = pool
       , metrics = MetricsState { accountRegister = accountRegister
                                , accountInsert = accountInsert
                                , accountTokenEncrypt = accountTokenEncrypt
                                }
       } <- ask
  P.observeDuration accountRegister $ do
    nowUTC <- liftIO getCurrentTime
    let now = utcTimeToPOSIXSeconds nowUTC
    uuid <- liftIO generateRandomUUID
    secret <- liftIO $! generateAccountSecret configSalt
    let hashedSecret = hashSBS configSalt unAccountSecret secret
        UUID rawUUID = uuid
        userNameHash = verifyDisplayName $! "user" <> (Text.decodeUtf8 $! BS.take 6 $! BS.fromShort rawUUID)
        person = Person
          { personCreationTime = now
          , personUuid = uuid
          , personLastSeenTime = now
          , personLastUpdated = now
          , personEmail = Nothing
          , personDisplayName = userNameHash
          , personHashedSecret = hashedSecret
          , personLoginsCount = 0
          }
    -- insert record into DB
    _ <- liftIO $! P.observeDuration accountInsert $ flip runSqlPersistMPool pool $ insert person
    -- if we are here then uuid and secret are unique
    token <- liftIO $ P.observeDuration accountTokenEncrypt $! ClientSession.encryptIO configAccountTokenEncryptionPrivateKey $! LBS.toStrict $! Aeson.encode (uuid, (0:: Word64) {- logins count is 0 for a new user -}) {- payload is of type (UUID Person, Word64) -}
    return $! RegisterResult
      { accountSecret = secret
      , accountToken = verifyAccountToken $! Text.decodeUtf8 token
      , personUUID = uuid
      }

-- | see OpEnergy.Account.API.V1.AccountV1API for reference of 'login' API call
-- 3 * O(ln n)
login :: AccountSecret -> AppM AccountToken
login secret = do
  State{ config = Config { configSalt = configSalt
                         , configAccountTokenEncryptionPrivateKey = configAccountTokenEncryptionPrivateKey
                         }
       , accountDBPool = pool
       , metrics = MetricsState { accountLogin = accountLogin
                                , accountTokenEncrypt = accountTokenEncrypt
                                , accountUpdateLoginsCount = accountUpdateLoginsCount
                                }
       } <- ask
  P.observeDuration accountLogin $ do
    let hashedSecret = hashSBS configSalt unAccountSecret secret
    mperson <- mgetPersonByHashedSecret hashedSecret
    case mperson of
      Nothing -> do
        let err = "ERROR: login: failed to find user account with given secret"
        runLogging $ $(logError) err
        throwJSON err400 err
      Just (Entity personKey person) -> do
        -- increase loginsCount returning new value
        loginsCount <- liftIO $! P.observeDuration accountUpdateLoginsCount $ flip runSqlPersistMPool pool $ do
          update personKey [ PersonLoginsCount =. (personLoginsCount person + 1) ]
          return (personLoginsCount person + 1)
        token <- liftIO $ P.observeDuration accountTokenEncrypt $! ClientSession.encryptIO configAccountTokenEncryptionPrivateKey $! LBS.toStrict $! Aeson.encode (personUuid person, loginsCount)
        return $! verifyAccountToken $! Text.decodeUtf8 token

-- | 2 * O(ln n): lookup + update. This function returns Nothing if there is no Person exist in DB with given hashed secret. Otherwise, it will update LastSeenTime to the current UTC time and returns (Entity Person)
mgetPersonByHashedSecret
  :: MonadIO m
  => Hashed AccountSecret
  -> AppT m (Maybe (Entity Person))
mgetPersonByHashedSecret hashedSecret = do
  State{ accountDBPool = pool
       , metrics = MetricsState { accountDBLookup = accountDBLookup
                                , accountMgetPersonByHashedSecret = accountMgetPersonByHashedSecret
                                }
       } <- ask
  liftIO $! P.observeDuration accountMgetPersonByHashedSecret $ do
    nowUTC <- liftIO getCurrentTime
    let now = utcTimeToPOSIXSeconds nowUTC
    liftIO $! P.observeDuration accountDBLookup $ flip runSqlPersistMPool pool $ do
      mperson <- selectFirst [ PersonHashedSecret ==. hashedSecret ] []
      case mperson of
        Nothing -> return Nothing
        Just (Entity key person) -> do
          update key ([ PersonLastSeenTime =. now ])
          return (Just (Entity key person { personLastSeenTime = now}))

-- | 2 * O(ln n): lookup + update. This function returns Nothing if there is no Person exist in DB with given display name. Otherwise, it will update LastSeenTime to the current UTC time and returns (Entity Person)
mgetPersonByDisplayName :: MonadIO m => DisplayName-> AppT m (Maybe (Entity Person))
mgetPersonByDisplayName displayName = do
  State{ accountDBPool = pool
       , metrics = MetricsState { accountDBLookup = accountDBLookup
                                }
       } <- ask
  liftIO $! P.observeDuration accountDBLookup $ do
    nowUTC <- liftIO getCurrentTime
    let now = utcTimeToPOSIXSeconds nowUTC
    P.observeDuration accountDBLookup $ flip runSqlPersistMPool pool $ do
      mperson <- selectFirst [ PersonDisplayName ==. displayName ] []
      case mperson of
        Nothing -> return Nothing
        Just (Entity key person) -> do
          update key [ PersonLastSeenTime =. now ]
          return (Just (Entity key person { personLastSeenTime = now}))

-- | 2 * O(ln n): lookup + update. This function returns Nothing if there is no Person exist in DB with given AccountToken. Otherwise, it will update LastSeenTime to the current UTC time and returns (Entity Person)
-- this function is expected to be used by other services in order to authenticate user
mgetPersonByAccountToken :: (MonadIO m, MonadMonitor m) => AccountToken-> AppT m (Maybe (Entity Person))
mgetPersonByAccountToken token = do
  State{ config = Config { configAccountTokenEncryptionPrivateKey = configAccountTokenEncryptionPrivateKey
                         }
       , accountDBPool = pool
       , metrics = MetricsState { accountMgetPersonByAccountToken = accountMgetPersonByAccountToken
                                , accountTokenDecrypt = accountTokenDecrypt
                                , accountDBLookup = accountDBLookup
                                }
       } <- ask
  P.observeDuration accountMgetPersonByAccountToken $! do
    mtuple <- P.observeDuration accountTokenDecrypt $! do
      case ClientSession.decrypt configAccountTokenEncryptionPrivateKey $! Text.encodeUtf8 (unAccountToken token) of
        Nothing-> return Nothing
        Just decrypted -> do
          let mtuple :: (Maybe (UUID Person, Word64)) = Aeson.decode $! LBS.fromStrict $! decrypted
          return mtuple
    case mtuple of
      Nothing-> return Nothing
      Just (uuid, loginsCount) -> do
        nowUTC <- liftIO getCurrentTime
        let now = utcTimeToPOSIXSeconds nowUTC
        P.observeDuration accountDBLookup $ liftIO $! flip runSqlPersistMPool pool $ do
          mperson <- selectFirst [ PersonUuid ==. uuid, PersonLoginsCount ==. loginsCount ] []
          case mperson of
            Nothing -> return Nothing
            Just (Entity key person) -> do
              update key [ PersonLastSeenTime =. now ]
              return (Just (Entity key person { personLastSeenTime = now}))

-- | see OpEnergy.Account.API.V1.AccountV1API for reference of 'displayname' API call
postDisplayName :: PostUserDisplayNameRequest -> AppM ()
postDisplayName (PostUserDisplayNameRequest token newName) = do
  State{ accountDBPool = pool
       , metrics = MetricsState { accountPostDisplayName = accountPostDisplayName
                                }
       } <- ask
  P.observeDuration accountPostDisplayName $ do
    mperson <- mgetPersonByAccountToken token
    case mperson of
      Nothing-> do
        let err = "ERROR: postDisplayName: invalid token"
        runLogging $ $(logError) err
        throwJSON err400 err
      Just ( Entity key _ ) -> do
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
            let err = "ERROR: postDisplayName: user with given display name already exists"
            runLogging $ $(logError) err
            throwJSON err400 err

-- | this function is being called on boot and supposed to be used to load some state from DB
loadDBState :: AppT IO ()
loadDBState = do
  -- TODO: maybe some cache fill and etc
  return ()
