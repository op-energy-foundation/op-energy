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

-- | API specifications of a backend service for Swagger
type AccountV1API
  = "register"
    :> Description "Registers new user and returns randomly generated account secret and account token.\n Account secret should be used for /login API encpoint.\n Account token should be used in the rest API calls as an authentication cookie"
    :> Post '[JSON] RegisterResult

  :<|> "login"
    :> ReqBody '[JSON] AccountSecret
    :> Description "Performs login with given account secret. Returns AccountToken value for being used with the rest API calls"
    :> Post '[JSON] AccountToken

  :<|> "user"
    :> "displayname"
    :> ReqBody '[JSON] PostUserDisplayNameRequest
    :> Description "Updates displayname for a given user"
    :> Post '[JSON] ()

  :<|> "git-hash"
    :> Description "returns short hash of commit of the op-energy git repo that had been used to build backend"
    :> Get '[JSON] GitHashResponse

type BlockTimeV1API
  = "ws"
    :> Description "websockets handler"
    :> WebSocket

  :<|> "future"
    :> "strike"
    :> Description ""
    :> Get '[JSON] [BlockTimeStrikeFuture]

  :<|> "future"
    :> "strike"
    :> Capture "BlockHeight" BlockHeight
    :> Capture "nLockTime" (Natural Int)
    :> Description ""
    :> Post '[JSON] ()

  :<|> "future"
    :> "strike"
    :> "guess"
    :> Capture "BlockHeight" BlockHeight
    :> Capture "nLockTime" (Natural Int)
    :> Description ""
    :> Get '[JSON] [BlockTimeStrikeGuessPublic]

  :<|> "future"
    :> "strike"
    :> "guess"
    :> Capture "BlockHeight" BlockHeight
    :> Capture "nLockTime" (Natural Int)
    :> Capture "guess" SlowFast
    :> Description ""
    :> Post '[JSON] ()

  :<|> "past"
    :> "strike"
    :> Description ""
    :> Get '[JSON] [BlockTimeStrikePast]

  :<|> "past"
    :> "strike"
    :> "guess"
    :> Description ""
    :> Get '[JSON] [BlockTimeStrikeGuessResultPublic]

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

