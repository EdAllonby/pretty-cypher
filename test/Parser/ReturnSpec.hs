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

runStandardParserReturnTests :: Spec
runStandardParserReturnTests = do
  it "parses return clause with all elements"
    $ "RETURN *" `shouldParseReturnQuery` Return ReturnAllElements
  it "parses return clause with single property"
    $ "RETURN n"
    `shouldParseReturnQuery` Return
      (ReturnProperties
         [Property (NestedObject (UnboundText "n") ObjectEnd) Nothing])
  it "parses return clause with multiple properties"
    $ "RETURN a AS Alias1, b AS Alias2, c"
    `shouldParseReturnQuery` Return
      (ReturnProperties
         [ Property
             (NestedObject (UnboundText "a") ObjectEnd)
             (Just (UnboundText "Alias1"))
         , Property
             (NestedObject (UnboundText "b") ObjectEnd)
             (Just (UnboundText "Alias2"))
         , Property (NestedObject (UnboundText "c") ObjectEnd) Nothing])
  it "parses return clause with nested properties"
    $ "RETURN a.b.c.d"
    `shouldParseReturnQuery` Return
      (ReturnProperties
         [ Property
             (NestedObject
                (UnboundText "a")
                (NestedObject
                   (UnboundText "b")
                   (NestedObject
                      (UnboundText "c")
                      (NestedObject (UnboundText "d") ObjectEnd))))
             Nothing])
  it "parses return clause with multiple nested properties"
    $ "RETURN a.b.c.d, e.f.g.h"
    `shouldParseReturnQuery` Return
      (ReturnProperties
         [ Property
             (NestedObject
                (UnboundText "a")
                (NestedObject
                   (UnboundText "b")
                   (NestedObject
                      (UnboundText "c")
                      (NestedObject (UnboundText "d") ObjectEnd))))
             Nothing
         , Property
             (NestedObject
                (UnboundText "e")
                (NestedObject
                   (UnboundText "f")
                   (NestedObject
                      (UnboundText "g")
                      (NestedObject (UnboundText "h") ObjectEnd))))
             Nothing])
  it "parses return clause with nested escaped properties"
    $ "RETURN `some object`.`@ n$st$d 0bject!`.unescapedText"
    `shouldParseReturnQuery` Return
      (ReturnProperties
         [ Property
             (NestedObject
                (BacktickedText "some object")
                (NestedObject
                   (BacktickedText "@ n$st$d 0bject!")
                   (NestedObject (UnboundText "unescapedText") ObjectEnd)))
             Nothing])
  it "parses return clause with aliased property"
    $ "RETURN n AS NumberOfEggs"
    `shouldParseReturnQuery` Return
      (ReturnProperties
         [ Property
             (NestedObject (UnboundText "n") ObjectEnd)
             (Just (UnboundText "NumberOfEggs"))])
  it "parses return clause with aliased literal property"
    $ "RETURN n AS `Number Of Eggs`"
    `shouldParseReturnQuery` Return
      (ReturnProperties
         [ Property
             (NestedObject (UnboundText "n") ObjectEnd)
             (Just (BacktickedText "Number Of Eggs"))])
  it
    "parses return clause with multiple nested properties and aliases and odd casing"
    $ "ReTuRn a.b.`c$$`.d aS ABCD, e.`f.g`.h As EFGH"
    `shouldParseReturnQuery` Return
      (ReturnProperties
         [ Property
             (NestedObject
                (UnboundText "a")
                (NestedObject
                   (UnboundText "b")
                   (NestedObject
                      (BacktickedText "c$$")
                      (NestedObject (UnboundText "d") ObjectEnd))))
             (Just (UnboundText "ABCD"))
         , Property
             (NestedObject
                (UnboundText "e")
                (NestedObject
                   (BacktickedText "f.g")
                   (NestedObject (UnboundText "h") ObjectEnd)))
             (Just (UnboundText "EFGH"))])
  it "parses return clause with literal item in double quotes"
    $ "RETURN \"I'm a literal\""
    `shouldParseReturnQuery` Return
      (ReturnProperties
         [ Property
             (NestedObject (QuotedText "I'm a literal") ObjectEnd)
             Nothing])

shouldParseReturnQuery :: Text -> Clause -> Expectation
shouldParseReturnQuery query expectedResult =
  parse parseReturn "" query `shouldParse` expectedResult
