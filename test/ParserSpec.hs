module ParserSpec where

import           Parser
import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec

runParserTests :: SpecWith ()
runParserTests = describe "Parser"
  $ do
    context "When parsing query"
      $ do
        it "parses match clause"
          $ parse parseQuery "" "MATCH (n:Node)"
          `shouldParse` Match (Node 'n' "Node") End
        it "parses match clause with extra spaces"
          $ do
            parse parseQuery "" "  MATCH  (  n:Node   )  "
              `shouldParse` Match (Node 'n' "Node") End
        it "fails on invalid clause producing correct error message"
          $ parse parseQuery "" "MARCH"
          `shouldFailWith` err 0 (utoks "MARCH" <> elabel "valid clause")