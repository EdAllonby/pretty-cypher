module Main where

import           Parser.Query (parseQuery)
import           Text.Megaparsec (parseTest)
import qualified Data.Text as T

main :: IO ()
main = getLine >>= parseTest parseQuery . T.pack
