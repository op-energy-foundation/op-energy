{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Aeson.Encode.Pretty as AP
import qualified Data.ByteString.Lazy.Char8 as BL8

import           Data.OpEnergy.Offer.API (apiSwagger)

main :: IO ()
main = BL8.putStrLn $ AP.encodePretty apiSwagger
