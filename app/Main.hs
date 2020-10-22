module Main where

import           Parser
import           Text.Megaparsec (parseTest)
import qualified Data.Text as T

main :: IO ()
main = getLine >>= parseTest parseQuery . T.pack
