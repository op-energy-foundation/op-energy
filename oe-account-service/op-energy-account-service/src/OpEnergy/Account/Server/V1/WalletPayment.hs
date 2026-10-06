{-- | This module defines the WalletPayment entity: one row per lightning
 - payment of an account, incoming (an invoice this service issued) or
 - outgoing (an invoice this service paid).
 -
 - An invoice exists before it is paid and a withdrawal can fail, so a
 - payment carries a status, while the ledger carries only what has already
 - moved a balance.
 -}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
module OpEnergy.Account.Server.V1.WalletPayment
  where

import           Data.Text (Text)
import           Data.Time.Clock.POSIX (POSIXTime)
import           GHC.Generics

import           Database.Persist
import           Database.Persist.Sql (PersistFieldSql(..))
import           Database.Persist.TH

import           Data.OpEnergy.Account.API.V1.Sats (Sats(..))
import           Data.OpEnergy.Account.API.V2.Bolt11Invoice
                 ( Bolt11Invoice(..), everifyBolt11Invoice
                 )
import           Data.OpEnergy.Account.API.V2.PaymentHash
                 ( PaymentHash(..), everifyPaymentHash
                 )

import           OpEnergy.Account.Server.V1.Person

-- | which way sats moved, or are expected to move
data WalletPaymentDirection
  = Incoming -- ^ an invoice this service issued, to be paid by someone else
  | Outgoing -- ^ an invoice this service paid on an account's behalf
  deriving (Show, Eq, Ord, Generic, Enum, Bounded)

-- | how far a payment has got
data WalletPaymentStatus
  = Pending -- ^ issued or in flight, the balance has not moved yet
  | Settled -- ^ paid, the balance has moved and the ledger has its entry
  | Failed -- ^ the payment did not go through, any stake is already back
  | Expired -- ^ nobody paid the invoice in time
  deriving (Show, Eq, Ord, Generic, Enum, Bounded)

walletPaymentDirectionToText :: WalletPaymentDirection -> Text
walletPaymentDirectionToText Incoming = "incoming"
walletPaymentDirectionToText Outgoing = "outgoing"

walletPaymentDirectionFromText :: Text -> Either Text WalletPaymentDirection
walletPaymentDirectionFromText "incoming" = Right Incoming
walletPaymentDirectionFromText "outgoing" = Right Outgoing
walletPaymentDirectionFromText other      =
  Left $ "WalletPaymentDirection: unknown direction: " <> other

walletPaymentStatusToText :: WalletPaymentStatus -> Text
walletPaymentStatusToText Pending = "pending"
walletPaymentStatusToText Settled = "settled"
walletPaymentStatusToText Failed  = "failed"
walletPaymentStatusToText Expired = "expired"

walletPaymentStatusFromText :: Text -> Either Text WalletPaymentStatus
walletPaymentStatusFromText "pending" = Right Pending
walletPaymentStatusFromText "settled" = Right Settled
walletPaymentStatusFromText "failed"  = Right Failed
walletPaymentStatusFromText "expired" = Right Expired
walletPaymentStatusFromText other     =
  Left $ "WalletPaymentStatus: unknown status: " <> other

share [mkPersist sqlSettings, mkMigrate "migrateWalletPayment"] [persistLowerCase|
WalletPayment
  personId PersonId
  direction WalletPaymentDirection
  status WalletPaymentStatus
  amountSats Sats
  feeSats Sats -- what the payment cost on top of its amount, 0 for incoming
  paymentHash PaymentHash
  invoice Bolt11Invoice
  note Text Maybe
  expiresAt POSIXTime Maybe -- incoming only: when the invoice stops being payable
  createdAt POSIXTime
  updatedAt POSIXTime
  UniqueWalletPaymentHash paymentHash -- the same payment can never be recorded twice
  deriving Eq Show Generic
|]

-- PersistField instances of the API types live in the service layer, as API
-- types should carry no Persist instances
instance PersistField WalletPaymentDirection where
  toPersistValue = toPersistValue . walletPaymentDirectionToText
  fromPersistValue v = fromPersistValue v >>= walletPaymentDirectionFromText
instance PersistFieldSql WalletPaymentDirection where
  sqlType _ = SqlString

instance PersistField WalletPaymentStatus where
  toPersistValue = toPersistValue . walletPaymentStatusToText
  fromPersistValue v = fromPersistValue v >>= walletPaymentStatusFromText
instance PersistFieldSql WalletPaymentStatus where
  sqlType _ = SqlString

instance PersistField PaymentHash where
  toPersistValue = toPersistValue . unPaymentHash
  fromPersistValue v = fromPersistValue v >>= everifyPaymentHash
instance PersistFieldSql PaymentHash where
  sqlType _ = SqlString

instance PersistField Bolt11Invoice where
  toPersistValue = toPersistValue . unBolt11Invoice
  fromPersistValue v = fromPersistValue v >>= everifyBolt11Invoice
instance PersistFieldSql Bolt11Invoice where
  sqlType _ = SqlString
