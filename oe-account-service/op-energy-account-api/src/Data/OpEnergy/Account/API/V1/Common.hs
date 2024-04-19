{-- | This module defines FilterRequest data type.
 -- The purpose is to be used to build search filters
 --}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleContexts           #-}
module Data.OpEnergy.Account.API.V1.Common
  ( jsonCommonOptions
  , commonParseJSON
  , commonToJSON
  ) where

import           Data.Aeson as A
import           Data.Aeson.Types as A
import qualified Data.Char as Char
import qualified Data.List as List
import           Data.Default
import           GHC.Generics

jsonCommonOptions :: Show a => a-> Options
jsonCommonOptions v = defaultOptions
    { fieldLabelModifier =
      (\s -> case s of
          [] -> []
          (first:rest)-> (Char.toLower first):rest
      ) . (List.drop $ List.length $ List.takeWhile (not . Char.isSpace) $ show v)
    , constructorTagModifier = List.map Char.toLower
    }

commonParseJSON :: (Show a, Default a, Generic a, GFromJSON Zero (Rep a)) => Value -> Parser a
commonParseJSON v = ret
  where
    ret = genericParseJSON (jsonCommonOptions fromParserV) v
    fromParserV = (fromParser ret)
    fromParser :: Default b => Parser b -> b
    fromParser = def

commonToJSON :: (Show a)=> (Options-> a-> r)-> a-> r
commonToJSON f v = f (jsonCommonOptions v) v
