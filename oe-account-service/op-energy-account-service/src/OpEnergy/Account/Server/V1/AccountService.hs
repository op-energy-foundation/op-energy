{-- | This module implements Account service in terms of OpEnergy.Account.API.V1.AccountV1API API, which you may
 - be insteresting to check for API description.
 -
 - Implementation details below
 -
 - 1. AccountToken's payload is currently implemented as tuple (uuid, loginsCount) encoded into JSON then encryped with AES (see Web.ClientSession from clientsession library) and then encoded into base64 form. Where:
 -  - uuid is of type UUID Person, which is unique string;
 -  - loginsCount - is of type Word64 and it is a monotonically increasing integer.
 - The reasoning behind such payload content is that it allowes us to consider AccountTokens to be valid only when loginsCount in the token matches loginsCounts field in DB record. It is supposed, that loginsCount DB record field will
will be updated each time user logins in with 'login' API call.
 - This way we are able to ensure, that:
 - 1. each user will get unique tokens within a range of [ 0; MAX Word64 value ];
 - 2. only 1 unique token is assumed to be valid;
 - 3. there is no possibility to use old leaked token to access any users other than owner of such token;
 - 4. when user will overflow MAX Word64 value of logins counts, he will start to get repeating tokens
 -}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE OverloadedStrings          #-}
module OpEnergy.Account.Server.V1.AccountService
  ( register
  , login
  , postDisplayName
  , loadDBState
  , mgetPersonByAccountToken -- supposed that another services will use this function to authenticate user
  ) where

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
      Nothing -> throwError err404
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
             


-- | see OpEnergy.Account.API.V1.AccountV1API for reference of 'register' API call
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
        let msg = "ERROR: postDisplayName: invalid token"
        runLogging $ $(logError) msg
        throwError err404
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
            let msg = "ERROR: postDisplayName: user with given display name already exists"
            runLogging $ $(logError) msg
            throwError err404

-- | performs read from DB in order to set State.currentHeightTip
loadDBState :: AppT IO ()
loadDBState = do
  -- TODO: maybe some cache fill and etc
  return ()
