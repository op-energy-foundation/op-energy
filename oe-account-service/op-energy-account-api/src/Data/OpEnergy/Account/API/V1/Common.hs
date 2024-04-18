{-- | This module defines FilterRequest data type.
 -- The purpose is to be used to build search filters
 --}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
module Data.OpEnergy.Account.API.V1.Common
  ( jsonCommonOptions
  ) where

import           Data.Aeson as A
import qualified Data.Char as Char
import qualified Data.List as List

jsonCommonOptions :: Show a => a-> Options
jsonCommonOptions v = defaultOptions
    { fieldLabelModifier =
      (\s -> case s of
          [] -> []
          (first:rest)-> (Char.toLower first):rest
      ) . (List.drop $ List.length $ List.takeWhile (not . Char.isSpace) $ show v)
    , constructorTagModifier = List.map Char.toLower
    }
