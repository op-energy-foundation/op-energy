{-- | Ties a telegram account to an op energy one, so the trade chat can
 - show both names.
 -
 - Connecting is the account owner's own doing and nothing happens without
 - it: a sender, who never connected, is shown by their telegram name alone,
 - which is what everybody is shown by to begin with.
 -
 - It is done with a one time code rather than by asking somebody to type
 - their username into telegram. The website makes a code for the account,
 - which is signed in, and puts it in a link to the bot; pressing Start
 - sends that code to the bot from the telegram account, which pressed it.
 - So the two are proved to be the same person by the act of pressing, and
 - nobody can claim an account they do not hold by guessing a name.
 -
 - The code is single use and short lived, because anybody who gets hold of
 - one before it is used could connect their own telegram to somebody
 - else's account.
 -}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
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
module OpEnergy.Offer.Server.V1.TelegramLink
  where

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Reader (ReaderT)
import           Data.Int (Int64)
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import           Data.Time.Clock (UTCTime)
import           GHC.Generics

import           Database.Persist
import           Database.Persist.Sql (SqlBackend)
import           Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateTelegramLink"] [persistLowerCase|
-- | one telegram account, which belongs to one op energy account
TelegramLink
  telegramUserId Int64
  personUUID Text -- the op energy account, as the cross service UUID rather than a local key: this service has no accounts table of its own
  opEnergyDisplayName Text -- their op energy name as it was when they connected. Not refreshed: this service is never told about a rename, so an account, which renames itself, keeps the old name in the chat until it disconnects and connects again
  linkedAt UTCTime
  UniqueTelegramLinkTelegram telegramUserId -- one telegram account cannot be claimed by two op energy accounts
  UniqueTelegramLinkPerson personUUID -- and one op energy account cannot claim two telegram accounts
  deriving Eq Show Generic

-- | a code handed out by the website and spent by pressing Start in telegram
TelegramLinkCode
  code Text
  personUUID Text
  opEnergyDisplayName Text -- taken when the code was made, so spending it needs no second lookup
  expiresAt UTCTime
  usedAt UTCTime Maybe -- single use: a code, which somebody else got hold of before it was spent, would connect their telegram to this account
  UniqueTelegramLinkCode code
  deriving Eq Show Generic
|]

-- | the op energy names of the given telegram accounts.
--
-- Read once for a whole page of the feed rather than once per message: a
-- page is twenty messages and they are mostly from the same few people.
--
-- Example:
--
-- > names <- opEnergyNamesTx [ 775566, 775567 ]
opEnergyNamesTx
  :: MonadIO m
  => [Int64]
  -> ReaderT SqlBackend m (Map.Map Int64 Text)
opEnergyNamesTx [] = return Map.empty
opEnergyNamesTx telegramUserIds = do
  links <- selectList [ TelegramLinkTelegramUserId <-. telegramUserIds ] []
  return $! Map.fromList
    [ ( telegramLinkTelegramUserId link, telegramLinkOpEnergyDisplayName link )
    | Entity _ link <- links
    ]

-- | stores a code for an account, replacing any it already had.
--
-- One unspent code per account, so asking twice does not leave the first
-- one lying around able to connect somebody else's telegram.
putLinkCodeTx
  :: MonadIO m
  => Text
  -> Text
  -> Text
  -> UTCTime
  -> ReaderT SqlBackend m ()
putLinkCodeTx code' personUUID' displayName expiresAt' = do
  deleteWhere [ TelegramLinkCodePersonUUID ==. personUUID' ]
  _ <- insert $! TelegramLinkCode
    { telegramLinkCodeCode = code'
    , telegramLinkCodePersonUUID = personUUID'
    , telegramLinkCodeOpEnergyDisplayName = displayName
    , telegramLinkCodeExpiresAt = expiresAt'
    , telegramLinkCodeUsedAt = Nothing
    }
  return ()

-- | spends a code and connects the two accounts, saying whether it worked.
--
-- Returns False for a code, which does not exist, has been spent, or has
-- expired, and for a telegram account, which is already connected to
-- another op energy account -- the last of these deliberately refuses
-- rather than moving the link, because moving it silently would let
-- somebody take a telegram account away from the account it belongs to.
redeemLinkCodeTx
  :: MonadIO m
  => Text
  -> Int64
  -> UTCTime
  -> ReaderT SqlBackend m Bool
redeemLinkCodeTx code' telegramUserId' now = do
  mcode <- getBy (UniqueTelegramLinkCode code')
  case mcode of
    Nothing -> return False
    Just (Entity codeKey codeRow)
      | telegramLinkCodeUsedAt codeRow /= Nothing -> return False
      | telegramLinkCodeExpiresAt codeRow < now -> return False
      | otherwise -> do
          mexisting <- getBy (UniqueTelegramLinkTelegram telegramUserId')
          case mexisting of
            Just _ -> return False
            Nothing -> do
              update codeKey [ TelegramLinkCodeUsedAt =. Just now ]
              mlink <- insertUnique $! TelegramLink
                { telegramLinkTelegramUserId = telegramUserId'
                , telegramLinkPersonUUID = telegramLinkCodePersonUUID codeRow
                , telegramLinkOpEnergyDisplayName =
                    telegramLinkCodeOpEnergyDisplayName codeRow
                , telegramLinkLinkedAt = now
                }
              -- Nothing here means this op energy account already has a
              -- telegram connected, which the unique on personUUID refuses
              return $! maybe False (const True) mlink

-- | disconnects an op energy account from whatever telegram account it had.
--
-- Connecting was the owner's choice, so undoing it has to be as well.
unlinkPersonTx
  :: MonadIO m
  => Text
  -> ReaderT SqlBackend m ()
unlinkPersonTx personUUID' = do
  deleteWhere [ TelegramLinkPersonUUID ==. personUUID' ]
  deleteWhere [ TelegramLinkCodePersonUUID ==. personUUID' ]
