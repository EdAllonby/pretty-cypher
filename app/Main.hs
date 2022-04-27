module Main where

import Cypher.Parser.Query (parseQuery)
import Data.Text qualified as T
import Text.Megaparsec (parseTest)

main :: IO ()
main = getLine >>= parseTest parseQuery . T.pack
