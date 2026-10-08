{-- | V2 wallet history handler: every change of the authenticated
 - account's balance, newest first.
 -
 - The rows come from the ledger, which is written whenever a balance moves,
 - so the history explains the balance rather than guessing at it.
 -}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE OverloadedStrings          #-}
module OpEnergy.Account.Server.V2.WalletAPI.GetTransactions
  ( getWalletTransactionsHandler
  , getWalletTransactions
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Logger(logError)
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Reader (ask)
import           Data.Int (Int64)
import qualified Data.List as List
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Crypto.Hash.SHA256 as SHA256
import           Data.Word (Word32)
import           Database.Persist
import           Database.Persist.Postgresql (runSqlPersistMPool)
import           Database.Persist.Sql (fromSqlKey)

import qualified Data.OpEnergy.Account.API.V1.Account as API
import           Data.OpEnergy.Account.API.V2.WalletTransactionsResult
                 ( WalletTransactionsResult(..)
                 )
import           Data.OpEnergy.Account.API.V1.Sats (Sats(..))
import           Data.OpEnergy.Account.API.V2.WalletTransaction
                 ( WalletTransaction(..)
                 )


import           OpEnergy.Account.Server.V1.AccountService
                 ( mgetPersonByAccountToken
                 )
import           OpEnergy.Account.Server.V1.Class
                 ( AppM, State(..), profile, runLogging
                 )
import           OpEnergy.Account.Server.V1.Config
                 ( Config(..)
                 )
import           OpEnergy.Account.Server.V1.LedgerEntry
import           OpEnergy.Error
                 ( eitherThrowJSON, runExceptPrefixT
                 , CallstackError, accountNotFound
                 )
import           OpEnergy.ExceptMaybe(exceptTMaybeT)

-- | V2 wallet/transactions endpoint
getWalletTransactionsHandler
  :: API.AccountToken
  -> Maybe Word32
  -> AppM WalletTransactionsResult
getWalletTransactionsHandler token mpage =
    let name = "V2.getWalletTransactionsHandler"
    in profile name $ eitherThrowJSON
      ( runLogging . $(logError))
      $ getWalletTransactions token mpage

-- | business logic for the V2 wallet/transactions endpoint. The total is
-- counted in the same transaction as the page is read, so the two can not
-- disagree and a client can number the pages
getWalletTransactions
  :: API.AccountToken
  -> Maybe Word32
  -> AppM (Either CallstackError WalletTransactionsResult)
getWalletTransactions token mpage =
    let name = "V2.getWalletTransactions"
    in profile name $ runExceptPrefixT name $ do
  (Entity key _) <- exceptTMaybeT accountNotFound
    $ mgetPersonByAccountToken token
  State{ config = config, accountDBPool = pool } <- lift ask
  let requestedPage = fromIntegral (fromMaybe 0 mpage) :: Int
      recordsPerReply = fromIntegral (configWalletRecordsPerPage config) :: Int
  -- the total is what lets a client show numbered pages rather than only a
  -- "next" step
  (total, entries) <- liftIO $ flip runSqlPersistMPool pool $ do
    totalV <- count [ LedgerEntryPersonId ==. key ]
    entriesV <- selectList
      [ LedgerEntryPersonId ==. key ]
      [ Desc LedgerEntryId
      , LimitTo recordsPerReply
      , OffsetBy (requestedPage * recordsPerReply)
      ]
    return (totalV, entriesV)
  return $! WalletTransactionsResult
    { results = List.map (renderWalletTransaction (configSalt config)) entries
    , page = fromIntegral requestedPage
    , pageSize = fromIntegral recordsPerReply
    , totalCount = fromIntegral total
    }

-- | an identifier for a ledger entry, which says nothing about the ledger.
--
-- The row's own key was reported, and ledger_entry's sequence is shared by
-- every account: from the gap between two of its own rows an account could
-- read how many rows every other account had written in between, which is a
-- platform-wide activity and volume oracle. Salted, so the small integers
-- behind it can not simply be hashed back
opaqueId :: Text -> Key LedgerEntry -> Text
opaqueId salt key = TE.decodeUtf8 $ B16.encode $ BS.take 16 $ SHA256.hash
  $ TE.encodeUtf8 (salt <> ":ledgerEntry:" <> Text.pack (show (fromSqlKey key)))

-- | Model -> API glue: the ledger stores a positive amount and the direction
-- it moved, while a client shows one signed number
renderWalletTransaction :: Text -> Entity LedgerEntry -> WalletTransaction
renderWalletTransaction salt (Entity key entry) = WalletTransaction
  { walletTransactionId = opaqueId salt key
  , walletTransactionAmountSats = signed (ledgerEntryAmountSats entry)
  , walletTransactionReason = ledgerEntryReason entry
  , walletTransactionBalanceAfter = ledgerEntryBalanceAfter entry
  , walletTransactionReference = ledgerEntryReference entry
  , walletTransactionCreatedAt = floor (ledgerEntryCreatedAt entry)
  }
  where
    signed :: Sats -> Int64
    signed (Sats amount) = case ledgerEntryDirection entry of
      Credit -> fromIntegral amount
      Debit -> negate (fromIntegral amount)
