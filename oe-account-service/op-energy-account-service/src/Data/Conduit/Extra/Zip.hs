module Data.Conduit.Extra.Zip
  ( zipConcatMapMC
  ) where

import           Data.Conduit (ConduitT)
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.Internal as C(zipSources)

-- | Given a function @f@, this @zipConcatMapMC f@ transforms a sequence @a_0, a_1, ...@ by zipping each @a_i@ with the values yielded by @f a_i@.
zipConcatMapMC
  :: Monad m
  => (a -> ConduitT () b m ())
  -> ConduitT a (a, b) m ()
zipConcatMapMC nested = C.awaitForever $ \v-> do
    C.toProducer $ C.zipSources
      (C.repeat v)
      (nested v)
