module Cypher.Parser.WithSpec (runParserWithTests) where

import Cypher.Parser.With (parseWith)
import Cypher.Types
import Data.Text as T (Text)
import Test.Hspec (Expectation, SpecWith, describe, it)
import Test.Hspec.Megaparsec (shouldParse)
import Text.Megaparsec (parse)

runParserWithTests :: SpecWith ()
runParserWithTests = describe "Cypher.Parser.With" $
  do
    it "parses single with clause with single literal text" $
      "WITH a"
        `shouldParseWithQuery` With [WithProperty (Property (TextValue (UnboundText "a")) Nothing)]
    it "parses single with clause with single literal aliased text" $
      "WITH a as Person"
        `shouldParseWithQuery` With [WithProperty (Property (TextValue (UnboundText "a")) (Just (UnboundText "Person")))]
    it "parses single with clause with multiple literal texts" $
      "WITH a, `b`"
        `shouldParseWithQuery` With
          [ WithProperty (Property (TextValue (UnboundText "a")) Nothing),
            WithProperty (Property (TextValue (BacktickedText "b")) Nothing)
          ]

shouldParseWithQuery :: Text -> Clause -> Expectation
shouldParseWithQuery query expectedResult =
  parse parseWith "" query `shouldParse` expectedResult