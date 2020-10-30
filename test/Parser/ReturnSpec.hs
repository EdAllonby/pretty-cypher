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
  it "parses return clause with all elements"
    $ "RETURN *" `shouldParseReturnQuery` Return AllElements
  it "parses return clause with single property"
    $ "RETURN n"
    `shouldParseReturnQuery` Return
      (Property (NestedObject (UnboundText "n") ObjectEnd) Nothing)
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
                  (NestedObject (UnboundText "d") ObjectEnd))))
         Nothing)
  it "parses return clause with nested escaped properties"
    $ "RETURN `some object`.`@ n$st$d 0bject!`.unescapedText"
    `shouldParseReturnQuery` Return
      (Property
         (NestedObject
            (BacktickedText "some object")
            (NestedObject
               (BacktickedText "@ n$st$d 0bject!")
               (NestedObject (UnboundText "unescapedText") ObjectEnd)))
         Nothing)
  it "parses return clause with aliased property"
    $ "RETURN n AS NumberOfEggs"
    `shouldParseReturnQuery` Return
      (Property
         (NestedObject (UnboundText "n") ObjectEnd)
         (Just (UnboundText "NumberOfEggs")))
  it "parses return clause with aliased literal property"
    $ "RETURN n AS `Number Of Eggs`"
    `shouldParseReturnQuery` Return
      (Property
         (NestedObject (UnboundText "n") ObjectEnd)
         (Just (BacktickedText "Number Of Eggs")))

shouldParseReturnQuery :: Text -> Clause -> Expectation
shouldParseReturnQuery query expectedResult =
  parse parseReturn "" query `shouldParse` expectedResult
