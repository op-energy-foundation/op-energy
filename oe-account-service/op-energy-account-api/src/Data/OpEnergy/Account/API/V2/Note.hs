{-- | A line of text, which an account attaches to an invoice it asks to be
 - paid.
 -}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Data.OpEnergy.Account.API.V2.Note
  ( Note(..)
  , defaultNote
  , maxNoteLength
  , everifyNote
  ) where

import           Data.Aeson
import           Data.Text                  (Text)
import qualified Data.Text as Text
import           Data.Typeable              (Typeable)
import           GHC.Generics
import           Control.Lens               ((&), (?~))
import           Data.Swagger

-- | what an account wrote on an invoice of its own
newtype Note = Note
  { unNote :: Text
  }
  deriving (Show, Eq, Ord, Generic, Typeable)

defaultNote :: Note
defaultNote = Note "for the hashrate derivative"

-- | the longest note an invoice may carry. The column is unbounded and the
-- whole body is read into memory before a handler sees it, so without a
-- bound one call stores as much as it is sent
maxNoteLength :: Int
maxNoteLength = 500

-- | checks the length of a note. 'FromJSON' goes through it, so an
-- unchecked note can not enter the service through the API
everifyNote :: Text -> Either Text Note
everifyNote raw
  | Text.length raw > maxNoteLength =
      Left "everifyNote: note is longer than allowed"
  | otherwise = Right (Note raw)

instance ToJSON Note where
  toJSON = toJSON . unNote
instance FromJSON Note where
  parseJSON = withText "Note"
    $ either (fail . Text.unpack) pure . everifyNote
instance ToSchema Note where
  declareNamedSchema _ = pure $ NamedSchema (Just "Note") $ mempty
    & type_ ?~ SwaggerString
    & description ?~
      ( "what an account writes on an invoice of its own, at most "
     <> Text.pack (show maxNoteLength) <> " characters"
      )
    & example ?~ toJSON defaultNote
