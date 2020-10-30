module Parser.ReturnSpec where

import           Types
import           Parser.Return
import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec
import           Data.Text as T

runParserReturnTests :: SpecWith ()
runParserReturnTests = describe "Parser.Return"
  $ do
    context "when parsing return query"
      $ do
        context "with standard clause" runStandardParserReturnTests

runStandardParserReturnTests = do
  it "parses return clause with single property"
    $ "RETURN n"
    `shouldParseReturnQuery` Return
      (Property (NestedObject (UnboundText "n") ObjectEnd))
  it "parses return clause with nested properties"
    $ "RETURN a.b.c.d"
    `shouldParseReturnQuery` Return
      (Property
         (NestedObject
            (UnboundText "a")
            (NestedObject
               (UnboundText "b")
               (NestedObject
                  (UnboundText "c")
                  (NestedObject (UnboundText "d") ObjectEnd)))))
  it "parses return clause with nested escaped properties"
    $ "RETURN `some object`.`@ n$st$d 0bject!`.unescapedText"
    `shouldParseReturnQuery` Return
      (Property
         (NestedObject
            (BacktickedText "some object")
            (NestedObject
               (BacktickedText "@ n$st$d 0bject!")
               (NestedObject (UnboundText "unescapedText") ObjectEnd))))
  it "parses return clause with all elements"
    $ "RETURN *" `shouldParseReturnQuery` Return AllElements

shouldParseReturnQuery :: Text -> Clause -> Expectation
shouldParseReturnQuery query expectedResult =
  parse parseReturn "" query `shouldParse` expectedResult
