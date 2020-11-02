module Cypher.Parser.DeleteSpec (runParserDeleteTests) where

import           Cypher.Types (LiteralText(BacktickedText, UnboundText)
                             , Clause(Delete))
import           Test.Hspec (describe, it, SpecWith, Expectation)
import           Test.Hspec.Megaparsec (shouldParse)
import           Text.Megaparsec (parse)
import           Data.Text as T (Text)
import           Cypher.Parser.Delete (parseDelete, parseDetachedDelete)

runParserDeleteTests :: SpecWith ()
runParserDeleteTests = describe "Cypher.Parser.Delete"
  $ do
    it "parses single delete clause with single literal text"
      $ "DELETE a" `shouldParseDeleteQuery` Delete False [UnboundText "a"]
    it "parses single delete clause with multiple literal texts"
      $ "DELETE a, `b`"
      `shouldParseDeleteQuery` Delete
        False
        [UnboundText "a", BacktickedText "b"]
    it "parses single detach delete clause with single literal text"
      $ "DETACH DELETE a"
      `shouldParseDetachedDeleteQuery` Delete True [UnboundText "a"]
    it "parses single detach delete clause with multiple literal texts"
      $ "DETACH DELETE a, `b`"
      `shouldParseDetachedDeleteQuery` Delete
        True
        [UnboundText "a", BacktickedText "b"]

shouldParseDeleteQuery :: Text -> Clause -> Expectation
shouldParseDeleteQuery query expectedResult =
  parse parseDelete "" query `shouldParse` expectedResult

shouldParseDetachedDeleteQuery :: Text -> Clause -> Expectation
shouldParseDetachedDeleteQuery query expectedResult =
  parse parseDetachedDelete "" query `shouldParse` expectedResult
