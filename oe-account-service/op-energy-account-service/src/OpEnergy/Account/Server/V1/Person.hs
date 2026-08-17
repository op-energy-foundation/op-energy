{-- | This module implements BlockTime strike service.
 -}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE DerivingStrategies         #-}
module OpEnergy.Account.Server.V1.Person
  where

import           Data.Time.Clock.POSIX(POSIXTime)
import qualified Data.List as List
import           Data.Word(Word64)
import qualified Data.ByteString.Short as BS
import qualified Data.Text.Encoding as Text
import           GHC.Generics

import qualified Web.ClientSession as ClientSession

import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Pagination
import           Database.Persist.TH


import qualified Data.OpEnergy.Account.API.V1.Account as AccountAPI
import qualified Data.OpEnergy.Account.API.V1.BlockTimeStrikeGuess as API
import qualified Data.OpEnergy.Account.API.V1.FilterRequest as API
import qualified Data.OpEnergy.Account.API.V1.Hash as API
import qualified Data.OpEnergy.Account.API.V1.UUID as API

share [mkPersist sqlSettings, mkMigrate "migrateAccount"] [persistLowerCase|
Person
  -- data
  uuid (API.UUID Person) -- will be used by other services as foreign key. local relations should use PersonId instead. If you in doubt why not use only Key, then think if you will be able to ensure that Key won't be changed in case of archieving persons, that haven't been seen for a long time.
  hashedSecret (API.Hashed AccountAPI.AccountSecret) -- hash of the secret in order to not to store plain secrets
  encryptedSecret AccountAPI.EncryptedAccountSecret Maybe -- the same secret, encrypted with configAccountTokenEncryptionPrivateKey rather than hashed, so that it can be decrypted and shown back to its owner. hashedSecret above stays the lookup key: encryption is not deterministic, so it cannot be searched by. Nothing for persons registered before this column existed, whose secret is only recoverable as a hash and so cannot be displayed
  loginsCount Word64 -- this field contains an integer value of how many times person had performed login. Default is 0
  email AccountAPI.EMailString Maybe -- can be empty (initially)
  displayName AccountAPI.DisplayName
  -- metadata
  creationTime POSIXTime
  lastSeenTime POSIXTime -- timestamp of the last seen time. By default the same as creationTime
  lastUpdated POSIXTime -- either CreationTime or last time of the lastest update
  -- constraints
  UniquePersonHashedSecret hashedSecret
  UniqueUUID uuid
  UniqueDisplayName displayName -- it will be confusing if we will allow persons with identical names
  deriving Eq Show Generic
|]

instance API.BuildFilter Person API.BlockTimeStrikeGuessFilter where
  sortOrder (filter, _) = maybe Descend id (API.blockTimeStrikeGuessFilterSort filter)
  buildFilter ( API.BlockTimeStrikeGuessFilter
                mPersonEQ
                mPersonNEQ
                -- guess
                _
                _
                -- observedResult
                _
                _
                -- strike block height
                _
                _
                _
                _
                -- strike strikeMediantime
                _
                _
                _
                _
                -- sort
                _
                _
                _ -- lines per page
              , _
              ) = List.concat
    [ maybe [] (\v-> [ PersonUuid ==. (API.UUID $! API.unUUID v) ]) mPersonEQ
    , maybe [] (\v-> [ PersonUuid !=. (API.UUID $! API.unUUID v) ]) mPersonNEQ
    ]

-- | the storable, recoverable form of an account secret: encrypted with the
-- service's account token key rather than hashed, so that it can be shown
-- back to its owner. Paired with 'decryptSecret'.
encryptSecret
  :: ClientSession.Key
  -> AccountAPI.AccountSecret
  -> IO AccountAPI.EncryptedAccountSecret
encryptSecret key secret =
  fmap (AccountAPI.EncryptedAccountSecret . Text.decodeUtf8)
    $! ClientSession.encryptIO key
    $! BS.fromShort $! AccountAPI.unAccountSecret secret

-- | recovers the plaintext secret stored by 'encryptSecret'. Nothing when the
-- ciphertext does not decrypt with the given key, which means the key has
-- changed since the row was written.
decryptSecret
  :: ClientSession.Key
  -> AccountAPI.EncryptedAccountSecret
  -> Maybe AccountAPI.AccountSecret
decryptSecret key encryptedSecret =
  fmap (AccountAPI.AccountSecret . BS.toShort)
    $! ClientSession.decrypt key
    $! Text.encodeUtf8 $! AccountAPI.unEncryptedAccountSecret encryptedSecret

apiModelUUIDPerson
  :: API.UUID Person
  -> API.UUID AccountAPI.Person
apiModelUUIDPerson = API.UUID . API.unUUID

modelApiUUIDPerson
  :: API.UUID AccountAPI.Person
  -> API.UUID Person
modelApiUUIDPerson = API.UUID . API.unUUID

modelApiPerson
  :: AccountAPI.Person
  -> Person
modelApiPerson v = Person
  { personUuid = modelApiUUIDPerson $ AccountAPI.uuid v
  , personHashedSecret = AccountAPI.hashedSecret v
  , personEncryptedSecret = Nothing -- the API-level Person deliberately does
    -- not carry the secret in any recoverable form, so it cannot round-trip
    -- through here
  , personLoginsCount = AccountAPI.loginsCount v
  , personEmail = AccountAPI.email v
  , personDisplayName = AccountAPI.displayName v
  , personCreationTime = AccountAPI.creationTime v
  , personLastSeenTime = AccountAPI.lastSeenTime v
  , personLastUpdated = AccountAPI.lastUpdated v
  }

apiModelPerson
  :: Person
  -> AccountAPI.Person
apiModelPerson v = AccountAPI.Person
  { AccountAPI.uuid = apiModelUUIDPerson $ personUuid v
  , AccountAPI.hashedSecret = personHashedSecret v
  , AccountAPI.loginsCount = personLoginsCount v
  , AccountAPI.email = personEmail v
  , AccountAPI.displayName = personDisplayName v
  , AccountAPI.creationTime = personCreationTime v
  , AccountAPI.lastSeenTime = personLastSeenTime v
  , AccountAPI.lastUpdated = personLastUpdated v
  }

