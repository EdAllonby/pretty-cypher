module ParserSpec where

import Parser
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

runParserTests :: SpecWith ()
runParserTests =
  describe "Parser" $ do
    it "returns correct result" $
      parse parseQuery "" "MATCH (n:Node)" `shouldParse` (Node 'n' "Node")