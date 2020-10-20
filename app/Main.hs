module Main where

import           Parser
import           Text.Megaparsec (parseTest)

main :: IO ()
main = parseTest parseQuery ""
