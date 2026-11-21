{-- | What the ban endpoint answers: which account was banned.
 -
 - The account is named back rather than only acknowledged, so the caller
 - can say whom it banned without holding on to what it sent, and so a ban
 - of the wrong account is visible in the answer.
 -}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DuplicateRecordFields      #-}
module Data.OpEnergy.Account.API.V2.BanUserResult
  ( BanUserResult(..)
  , defaultBanUserResult
  ) where

import           Data.Swagger
import           Control.Lens
import           GHC.Generics
import           Data.Typeable              (Typeable)
import           Data.Aeson

import           Data.OpEnergy.Account.API.V1.Account
                 ( Person, DisplayName, defaultDisplayName
                 )
import           Data.OpEnergy.Account.API.V1.UUID
                 ( UUID, defaultUUID
                 )

-- | the account, which is now banned
data BanUserResult = BanUserResult
  { personUUID  :: UUID Person
  , displayName :: DisplayName
  }
  deriving (Show, Generic, Typeable)

defaultBanUserResult :: BanUserResult
defaultBanUserResult = BanUserResult defaultUUID defaultDisplayName

instance ToJSON BanUserResult
instance FromJSON BanUserResult
instance ToSchema BanUserResult where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "BanUserResult schema"
    & mapped.schema.example ?~ toJSON defaultBanUserResult
