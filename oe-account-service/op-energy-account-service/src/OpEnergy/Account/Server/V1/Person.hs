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
import           GHC.Generics

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

instance API.BuildFilter Person API.BlockTimeStrikeGuessResultPublicFilter where
  sortOrder (filter, _) = maybe Descend id (API.blockTimeStrikeGuessResultPublicFilterSort filter)
  buildFilter ( API.BlockTimeStrikeGuessResultPublicFilter
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

