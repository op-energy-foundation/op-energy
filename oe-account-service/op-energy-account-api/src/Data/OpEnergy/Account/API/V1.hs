{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DuplicateRecordFields      #-}
module Data.OpEnergy.Account.API.V1 where

import           Data.Swagger
import           Control.Lens
import           GHC.Generics
import           Data.Typeable              (Typeable)
import           Data.Aeson

import           Servant.API
import           Servant.API.WebSocket (WebSocket)

import           Data.OpEnergy.API.V1(GitHashResponse)
import           Data.OpEnergy.API.V1.Block
import           Data.OpEnergy.API.V1.Natural
import           Data.OpEnergy.Account.API.V1.Account
import           Data.OpEnergy.Account.API.V1.BlockTimeStrike
import           Data.OpEnergy.Account.API.V1.BlockTimeStrikeGuess

-- | API specifications of a backend service for Swagger
type AccountV1API
  = "register"
    :> Description "Registers new user and returns randomly generated account secret and account token.\n Account secret should be used for /login API encpoint.\n Account token should be used in the rest API calls as an authentication cookie. Current authentication schema is based on the following assumptions: 1. frontend should have /login/$secret endpoint. User should access frontend with $frontend_url/login/$secret. This way frontend will be able to perform /api/v1/account/login call and store returned AccountToken value in the state service for the future use and then, redirect to the main page; 2. if user is accessing main page without stored account token (ie, not by redirect from step 1), then frontend should call /api/v1/account/register API call and display message to user \"Use this link to access your account in the future\", where link should point to $frontend/login/$secret"
    :> Post '[JSON] RegisterResult

  :<|> "login"
    :> ReqBody '[JSON] AccountSecret
    :> Description "Performs login with given account secret. Returns AccountToken value for being used with the rest API calls. See 'register' API call description for the reference of expected frontend's behavior related to secrets and tokens"
    :> Post '[JSON] AccountToken

  :<|> "displayname"
    :> ReqBody '[JSON] PostUserDisplayNameRequest
    :> Description "Updates displayname for a given user"
    :> Post '[JSON] ()

  :<|> "git-hash"
    :> Description "returns short hash of commit of the op-energy git repo that had been used to build backend"
    :> Get '[JSON] GitHashResponse

type BlockTimeV1API
  = "ws"
    :> Description "websockets handler. The goal is to be able to recieve notifications about newly created blocktime strikes and/or guesses"
    :> WebSocket

  :<|> "future"
    :> "strike"
    :> ReqBody '[JSON] AccountToken -- require authentication
    :> Description "returns list of the future time strikes. Requires authentication."
    :> Post '[JSON] [BlockTimeStrikeFuture]

  :<|> "future"
    :> "strike"
    :> ReqBody '[JSON] AccountToken -- require authentication
    :> Capture "BlockHeight" BlockHeight
    :> Capture "nLockTime" (Natural Int)
    :> Description "Creates new future time strike by given BlockHeight and nLockTime. Requires authentication. Where: BlockHeight - height of the block in the future. It is expected, that it should be at least at 12 block in the future than current confirmed tip. nLockTime is a POSIX time in the future."
    :> Post '[JSON] ()

  :<|> "future"
    :> "strike"
    :> "guess"
    :> ReqBody '[JSON] AccountToken -- require authentication
    :> Capture "BlockHeight" BlockHeight
    :> Capture "nLockTime" (Natural Int)
    :> Description "returns list of the guesses for a given future time strike. Requires authentication."
    :> Post '[JSON] [BlockTimeStrikeGuessPublic]

  :<|> "future"
    :> "strike"
    :> "guess"
    :> ReqBody '[JSON] AccountToken -- require authentication
    :> Capture "BlockHeight" BlockHeight
    :> Capture "nLockTime" (Natural Int)
    :> Capture "guess" SlowFast
    :> Description "creates a guess for the given future time strike. Requires authentication."
    :> Post '[JSON] ()

  :<|> "past"
    :> "strike"
    :> ReqBody '[JSON] AccountToken -- require authentication
    :> Description "returns list of past strikes, that have been already processed. Requires authentication. Time strike becomes \"past\" when it becomes confirmed and you can think about it as archived strike, that had been processed and now being kept as history"
    :> Post '[JSON] [BlockTimeStrikePast]

  :<|> "past"
    :> "strike"
    :> "guess"
    :> ReqBody '[JSON] AccountToken -- require authentication
    :> Capture "BlockHeight" BlockHeight
    :> Capture "nLockTime" (Natural Int)
    :> Description "returns results for the given blocktime strike in the past. Requires authentication. Time strike becomes \"past\" when it becomes confirmed and you can think about it as archived strike, that had been processed and now being kept as history. 'Guess' becomes a 'result' when blocktime strike becomes confirmed and processed."
    :> Post '[JSON] [BlockTimeStrikeGuessResultPublic]

  :<|> "git-hash"
    :> Description "returns short hash of commit of the op-energy git repo that had been used to build backend"
    :> Get '[JSON] GitHashResponse

type FakeWSAPI = Get '[JSON] ()

data RegisterResult = RegisterResult
  { accountSecret :: AccountSecret
  , accountToken  :: AccountToken
  }
  deriving (Show, Generic, Typeable)

defaultRegisterResult :: RegisterResult
defaultRegisterResult = RegisterResult defaultAccountSecret defaultAccountToken

instance ToJSON RegisterResult
instance FromJSON RegisterResult
instance ToSchema RegisterResult where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "RegisterResult schema"
    & mapped.schema.example ?~ toJSON defaultRegisterResult

data PostUserDisplayNameRequest = PostUserDisplayNameRequest
  { account_token :: AccountToken
  , display_name :: DisplayName
  }
  deriving (Show, Generic, Typeable)
defaultPostUserDisplayNameRequest :: PostUserDisplayNameRequest
defaultPostUserDisplayNameRequest = PostUserDisplayNameRequest
  { account_token = defaultAccountToken
  , display_name = verifyDisplayName "newUserName"
  }
instance ToJSON PostUserDisplayNameRequest
instance FromJSON PostUserDisplayNameRequest
instance ToSchema PostUserDisplayNameRequest where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "PostUserDisplayNameRequest schema"
    & mapped.schema.example ?~ toJSON defaultPostUserDisplayNameRequest

