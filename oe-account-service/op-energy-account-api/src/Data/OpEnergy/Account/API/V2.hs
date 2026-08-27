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
module Data.OpEnergy.Account.API.V2 where

import           Data.Swagger
import           Control.Lens
import           GHC.Generics
import           Data.Typeable              (Typeable)
import           Data.Aeson
import           Data.Text                  (Text)
import           Data.Word                  (Word64)

import           Servant.API

import           Data.OpEnergy.Account.API.V1.Account
import           Data.OpEnergy.Account.API.V1.UUID

-- | API specifications of a backend service for Swagger
type AccountV2API
  = "login"
    :> ReqBody '[JSON] AccountSecret
    :> Description "Performs login with given account secret. Returns LoginResult(token and person UUID) value for being used with the rest API calls. See 'register' API call description for the reference of expected frontend's behavior related to secrets and tokens"
    :> Post '[JSON] LoginResult

  :<|> "whoami"
    :> Header'
       '[ Required
        , Strict
        , Description "Account token gotten from /login or /register"
        ]
        "Authorization"
        AccountToken -- require authentication
    :> Description "Resolves an account token to the account's stable cross-service identity (personUUID), current display name, and current sandbox wallet balance. Meant for other op-energy services (e.g. oe-offer-service, via Data.OpEnergy.Account.Client) that need to verify a token they were handed and attribute something to the account it belongs to -- not for browser clients, which have no other use for this yet on V2 (see AccountV1API's register/login/displayname for the user-facing flow this predates)."
    :> Get '[JSON] WhoAmIResult

  :<|> "internal" :> "balance" :> "deduct"
    :> Header'
       '[ Required
        , Strict
        , Description "shared secret identifying an internal, service-to-service caller -- see configInternalServiceSharedSecret. Never accepted from, or meant to be exposed to, a browser client."
        ]
        "X-Internal-Service-Secret"
        Text
    :> ReqBody '[JSON] BalanceAdjustRequest
    :> Description "Atomically deducts amountSats from the given account's balance, failing (400) if it would go negative. Internal-only -- see the X-Internal-Service-Secret header. oe-offer-service uses this to stake a posted offer's makerStakeSats."
    :> Post '[JSON] BalanceAdjustResult

  :<|> "internal" :> "balance" :> "credit"
    :> Header'
       '[ Required
        , Strict
        , Description "shared secret identifying an internal, service-to-service caller -- see configInternalServiceSharedSecret. Never accepted from, or meant to be exposed to, a browser client."
        ]
        "X-Internal-Service-Secret"
        Text
    :> ReqBody '[JSON] BalanceAdjustRequest
    :> Description "Unconditionally credits amountSats to the given account's balance. Internal-only -- see the X-Internal-Service-Secret header. oe-offer-service uses this to refund a cancelled/expired offer's stake."
    :> Post '[JSON] BalanceAdjustResult

data WhoAmIResult = WhoAmIResult
  { personUUID :: UUID Person
  , displayName :: DisplayName
  , balance :: Word64
  }
  deriving (Show, Generic, Typeable)

defaultWhoAmIResult :: WhoAmIResult
defaultWhoAmIResult = WhoAmIResult defaultUUID defaultDisplayName 300000

instance ToJSON WhoAmIResult
instance FromJSON WhoAmIResult
instance ToSchema WhoAmIResult where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "WhoAmIResult schema"
    & mapped.schema.example ?~ toJSON defaultWhoAmIResult

-- | body of the 'internal/balance/deduct' and 'internal/balance/credit' API
-- calls -- see those routes for what each does with it.
data BalanceAdjustRequest = BalanceAdjustRequest
  { personUUID :: UUID Person
  , amountSats :: Word64
    -- ^ always given as a positive magnitude -- deduct subtracts it
    -- (failing rather than going negative), credit adds it. Which one
    -- happens is which route was called, not a sign on this field.
  }
  deriving (Show, Generic, Typeable)

defaultBalanceAdjustRequest :: BalanceAdjustRequest
defaultBalanceAdjustRequest = BalanceAdjustRequest defaultUUID 50000

instance ToJSON BalanceAdjustRequest
instance FromJSON BalanceAdjustRequest
instance ToSchema BalanceAdjustRequest where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "BalanceAdjustRequest schema"
    & mapped.schema.example ?~ toJSON defaultBalanceAdjustRequest

-- | result of the 'internal/balance/deduct' and 'internal/balance/credit'
-- API calls: the account's balance after the adjustment.
data BalanceAdjustResult = BalanceAdjustResult
  { balance :: Word64
  }
  deriving (Show, Generic, Typeable)

defaultBalanceAdjustResult :: BalanceAdjustResult
defaultBalanceAdjustResult = BalanceAdjustResult 250000

instance ToJSON BalanceAdjustResult
instance FromJSON BalanceAdjustResult
instance ToSchema BalanceAdjustResult where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "BalanceAdjustResult schema"
    & mapped.schema.example ?~ toJSON defaultBalanceAdjustResult

data LoginResult = LoginResult
  { accountToken  :: AccountToken
  , personUUID :: UUID Person
  }
  deriving (Show, Generic, Typeable)

defaultLoginResult :: LoginResult
defaultLoginResult = LoginResult defaultAccountToken defaultUUID

instance ToJSON LoginResult
instance FromJSON LoginResult
instance ToSchema LoginResult where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "LoginResult schema"
    & mapped.schema.example ?~ toJSON defaultLoginResult


