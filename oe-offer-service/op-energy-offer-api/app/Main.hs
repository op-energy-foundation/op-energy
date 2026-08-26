module Main where
import           Data.OpEnergy.Offer.API
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS

main :: IO ()
main = do
  BS.putStrLn $ encode apiSwagger
  return ()
