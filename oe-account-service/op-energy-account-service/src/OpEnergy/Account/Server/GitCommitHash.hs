{--
 - this module provides git commit hash. It is supposed that build system should replace GIT_COMMIT_HASH value with actual hash value and thus, "GIT_COMMIT_HASH" will be used as value only when backend had been built manually
 -}
module OpEnergy.Account.Server.GitCommitHash
  ( gitCommitHash
  ) where

import           Data.Text

gitCommitHash :: Text
gitCommitHash = "GIT_COMMIT_HASH"
