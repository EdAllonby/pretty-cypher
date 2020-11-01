module Cypher.Parser.ReturnSpec (runParserReturnTests) where

import           Cypher.Types
import           Cypher.Parser.Return
import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec
import           Data.Text as T
import qualified Data.Map as M

runParserReturnTests :: SpecWith ()
runParserReturnTests = describe "Cypher.Parser.Return"
  $ context "when parsing return query"
  $ do
    context "with standard clause" runStandardParserReturnTests

runStandardParserReturnTests :: Spec
runStandardParserReturnTests = do
  it "parses return clause with all elements"
    $ "RETURN *" `shouldParseReturnQuery` Return ReturnAllElements
  it "parses return clause with single property"
    $ "RETURN n"
    `shouldParseReturnQuery` Return
      (ReturnExpressions
         [ ReturnProperty
             (Property (NestedObject (UnboundText "n") ObjectEnd) Nothing)])
  it "parses return clause with multiple properties"
    $ "RETURN a AS Alias1, b AS Alias2, c"
    `shouldParseReturnQuery` Return
      (ReturnExpressions
         [ ReturnProperty
             (Property
                (NestedObject (UnboundText "a") ObjectEnd)
                (Just (UnboundText "Alias1")))
         , ReturnProperty
             (Property
                (NestedObject (UnboundText "b") ObjectEnd)
                (Just (UnboundText "Alias2")))
         , ReturnProperty
             (Property (NestedObject (UnboundText "c") ObjectEnd) Nothing)])
  it "parses return clause with nested properties"
    $ "RETURN a.b.c.d"
    `shouldParseReturnQuery` Return
      (ReturnExpressions
         [ ReturnProperty
             (Property
                (NestedObject
                   (UnboundText "a")
                   (NestedObject
                      (UnboundText "b")
                      (NestedObject
                         (UnboundText "c")
                         (NestedObject (UnboundText "d") ObjectEnd))))
                Nothing)])
  it "parses return clause with multiple nested properties"
    $ "RETURN a.b.c.d, e.f.g.h"
    `shouldParseReturnQuery` Return
      (ReturnExpressions
         [ ReturnProperty
             (Property
                (NestedObject
                   (UnboundText "a")
                   (NestedObject
                      (UnboundText "b")
                      (NestedObject
                         (UnboundText "c")
                         (NestedObject (UnboundText "d") ObjectEnd))))
                Nothing)
         , ReturnProperty
             (Property
                (NestedObject
                   (UnboundText "e")
                   (NestedObject
                      (UnboundText "f")
                      (NestedObject
                         (UnboundText "g")
                         (NestedObject (UnboundText "h") ObjectEnd))))
                Nothing)])
  it "parses return clause with nested escaped properties"
    $ "RETURN `some object`.`@ n$st$d 0bject!`.unescapedText"
    `shouldParseReturnQuery` Return
      (ReturnExpressions
         [ ReturnProperty
             (Property
                (NestedObject
                   (BacktickedText "some object")
                   (NestedObject
                      (BacktickedText "@ n$st$d 0bject!")
                      (NestedObject (UnboundText "unescapedText") ObjectEnd)))
                Nothing)])
  it "parses return clause with aliased property"
    $ "RETURN n AS NumberOfEggs"
    `shouldParseReturnQuery` Return
      (ReturnExpressions
         [ ReturnProperty
             (Property
                (NestedObject (UnboundText "n") ObjectEnd)
                (Just (UnboundText "NumberOfEggs")))])
  it "parses return clause with aliased literal property"
    $ "RETURN n AS `Number Of Eggs`"
    `shouldParseReturnQuery` Return
      (ReturnExpressions
         [ ReturnProperty
             (Property
                (NestedObject (UnboundText "n") ObjectEnd)
                (Just (BacktickedText "Number Of Eggs")))])
  it
    "parses return clause with multiple nested properties and aliases and odd casing"
    $ "ReTuRn a.b.`c$$`.d aS ABCD, e.`f.g`.h As EFGH"
    `shouldParseReturnQuery` Return
      (ReturnExpressions
         [ ReturnProperty
             (Property
                (NestedObject
                   (UnboundText "a")
                   (NestedObject
                      (UnboundText "b")
                      (NestedObject
                         (BacktickedText "c$$")
                         (NestedObject (UnboundText "d") ObjectEnd))))
                (Just (UnboundText "ABCD")))
         , ReturnProperty
             (Property
                (NestedObject
                   (UnboundText "e")
                   (NestedObject
                      (BacktickedText "f.g")
                      (NestedObject (UnboundText "h") ObjectEnd)))
                (Just (UnboundText "EFGH")))])
  it "parses return clause with literal item in double quotes"
    $ "RETURN \"I'm a literal\""
    `shouldParseReturnQuery` Return
      (ReturnExpressions
         [ ReturnProperty
             (Property
                (NestedObject (QuotedText "I'm a literal") ObjectEnd)
                Nothing)])
  it "parses return clause with pattern"
    $ "RETURN (a)-->()"
    `shouldParseReturnQuery` Return
      (ReturnExpressions
         [ ReturnPattern
             (Pattern
                Nothing
                Nothing
                [ Node (AnyNode (UnboundText "a")) M.empty
                , ConnectorDirection AnonymousRightDirection
                , Node EmptyNode M.empty])])

shouldParseReturnQuery :: Text -> Clause -> Expectation
shouldParseReturnQuery query expectedResult =
  parse parseReturn "" query `shouldParse` expectedResult
