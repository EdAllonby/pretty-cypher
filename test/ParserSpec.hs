{-# LANGUAGE QuasiQuotes #-}

module ParserSpec where

import           Types
import           Parser
import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec
import           Data.Text as T
import qualified Data.Map as M
import           Text.RawString.QQ (r)

runParserTests :: SpecWith ()
runParserTests = describe "Parser"
  $ do
    context "when parsing match only query"
      $ do
        context "with node" runParserMatchNodeTests
        context "with relationship" runParserMatchRelationshipTests
        context "with direction" runParserMatchDirectionTests
        context "with pattern cases" runParserMatchPatternTests
        context "with error" runParserMatchErrorTests
    context "when parsing optional match only query"
      $ do
        runParserOptionalMatchTests

runParserMatchNodeTests = do
  it "parses match clause with node"
    $ "MATCH (per:Person) RETURN"
    `shouldParseQuery` [ Match
                           [ Pattern
                               Nothing
                               [ Node
                                   (LabelledNode (Just "per") ["Person"])
                                   M.empty]]
                       , Return]
  it "parses match clause with multi labelled node"
    $ "MATCH (per:Person:Actor) RETURN"
    `shouldParseQuery` [ Match
                           [ Pattern
                               Nothing
                               [ Node
                                   (LabelledNode
                                      (Just "per")
                                      ["Person", "Actor"])
                                   M.empty]]
                       , Return]
  it "parses match clause with labelled node specifying properties"
    $ "MATCH (per:Person { name: ' D. A. V. E ', age: 32, height: 1.6, delta: -10, base: -3.14 }) RETURN"
    `shouldParseQuery` [ Match
                           [ Pattern
                               Nothing
                               [ Node
                                   (LabelledNode (Just "per") ["Person"])
                                   (M.fromList
                                      [ ("age", IntegerValue 32)
                                      , ("base", DoubleValue (-3.14))
                                      , ("delta", IntegerValue (-10))
                                      , ("height", DoubleValue 1.6)
                                      , ("name", TextValue " D. A. V. E ")])]]
                       , Return]
  it "parses match clause with anonymous node"
    $ "MATCH (:Person) RETURN"
    `shouldParseQuery` [ Match
                           [ Pattern
                               Nothing
                               [Node (LabelledNode Nothing ["Person"]) M.empty]]
                       , Return]
  it "parses match clause with anonymous node specifying properties"
    $ "MATCH (:Person { name: ' D. A. V. E ', age: 32, height: 1.6, delta: -10, base: -3.14 }) RETURN"
    `shouldParseQuery` [ Match
                           [ Pattern
                               Nothing
                               [ Node
                                   (LabelledNode Nothing ["Person"])
                                   (M.fromList
                                      [ ("age", IntegerValue 32)
                                      , ("base", DoubleValue (-3.14))
                                      , ("delta", IntegerValue (-10))
                                      , ("height", DoubleValue 1.6)
                                      , ("name", TextValue " D. A. V. E ")])]]
                       , Return]
  it "parses match clause with empty node"
    $ "MATCH () RETURN"
    `shouldParseQuery` [ Match [Pattern Nothing [Node EmptyNode M.empty]]
                       , Return]
  it "parses match clause with empty node specifying properties"
    $ "MATCH ({ name: ' D. A. V. E ', age: 32, height: 1.6, delta: -10, base: -3.14 }) RETURN"
    `shouldParseQuery` [ Match
                           [ Pattern
                               Nothing
                               [ Node
                                   EmptyNode
                                   (M.fromList
                                      [ ("age", IntegerValue 32)
                                      , ("base", DoubleValue (-3.14))
                                      , ("delta", IntegerValue (-10))
                                      , ("height", DoubleValue 1.6)
                                      , ("name", TextValue " D. A. V. E ")])]]
                       , Return]
  it "parses match clause with any node"
    $ "MATCH (n) RETURN"
    `shouldParseQuery` [ Match [Pattern Nothing [Node (AnyNode "n") M.empty]]
                       , Return]
  it "parses match clause with any node specifying properties"
    $ "MATCH (n{ name: ' D. A. V. E ', age: 32, height: 1.6, delta: -10, base: -3.14 }) RETURN"
    `shouldParseQuery` [ Match
                           [ Pattern
                               Nothing
                               [ Node
                                   (AnyNode "n")
                                   (M.fromList
                                      [ ("age", IntegerValue 32)
                                      , ("base", DoubleValue (-3.14))
                                      , ("delta", IntegerValue (-10))
                                      , ("height", DoubleValue 1.6)
                                      , ("name", TextValue " D. A. V. E ")])]]
                       , Return]

runParserOptionalMatchTests = do
  it "parses optional match clause with node"
    $ "OPTIONAL MATCH (per:Person) RETURN"
    `shouldParseQuery` [ OptionalMatch
                           [ Pattern
                               Nothing
                               [ Node
                                   (LabelledNode (Just "per") ["Person"])
                                   M.empty]]
                       , Return]
  it "parses match with optional match clause"
    $ [r|
MATCH (a:Movie { title: 'Wall Street' })
OPTIONAL MATCH (a)-[r:ACTS_IN]->()
RETURN
    |]
    `shouldParseQuery` [ Match
                           [ Pattern
                               Nothing
                               [ Node
                                   (LabelledNode (Just "a") ["Movie"])
                                   (M.fromList
                                      [("title", TextValue "Wall Street")])]]
                       , OptionalMatch
                           [ Pattern
                               Nothing
                               [ Node
                                   (AnyNode { anyNodeVariable = "a" })
                                   M.empty
                               , ConnectorDirection NoDirection
                               , Relationship
                                   (LabelledRelationship { labelledRelationshipVariable =
                                                             Just "r"
                                                         , labelledRelationshipLabel = "ACTS_IN"
                                                         })
                                   M.empty
                               , ConnectorDirection RightDirection
                               , Node EmptyNode M.empty]]
                       , Return]

runParserMatchRelationshipTests = do
  it "parses match clause with relationship"
    $ "MATCH [fo:FOLLOWS] RETURN"
    `shouldParseQuery` [ Match
                           [ Pattern
                               Nothing
                               [ Relationship
                                   (LabelledRelationship (Just "fo") "FOLLOWS")
                                   M.empty]]
                       , Return]
  it "parses match clause with screaming snake case relationship"
    $ "MATCH [i:IS_A_FAN_OF] RETURN"
    `shouldParseQuery` [ Match
                           [ Pattern
                               Nothing
                               [ Relationship
                                   (LabelledRelationship
                                      (Just "i")
                                      "IS_A_FAN_OF")
                                   M.empty]]
                       , Return]
  it "parses match clause with relationship specifying properties"
    $ "MATCH [fo:FOLLOWS { name: ' D. A. V. E ', age: 32, height: 1.6, delta: -10, base: -3.14 }] RETURN"
    `shouldParseQuery` [ Match
                           [ Pattern
                               Nothing
                               [ Relationship
                                   (LabelledRelationship (Just "fo") "FOLLOWS")
                                   (M.fromList
                                      [ ("age", IntegerValue 32)
                                      , ("base", DoubleValue (-3.14))
                                      , ("delta", IntegerValue (-10))
                                      , ("height", DoubleValue 1.6)
                                      , ("name", TextValue " D. A. V. E ")])]]
                       , Return]
  it "parses match clause with anonymous relationship"
    $ "MATCH [:FOLLOWS] RETURN"
    `shouldParseQuery` [ Match
                           [ Pattern
                               Nothing
                               [ Relationship
                                   (LabelledRelationship Nothing "FOLLOWS")
                                   M.empty]]
                       , Return]
  it "parses match clause with anonymous relationship specifying properties"
    $ "MATCH [:FOLLOWS { name: ' D. A. V. E ', age: 32, height: 1.6, delta: -10, base: -3.14 }] RETURN"
    `shouldParseQuery` [ Match
                           [ Pattern
                               Nothing
                               [ Relationship
                                   (LabelledRelationship Nothing "FOLLOWS")
                                   (M.fromList
                                      [ ("age", IntegerValue 32)
                                      , ("base", DoubleValue (-3.14))
                                      , ("delta", IntegerValue (-10))
                                      , ("height", DoubleValue 1.6)
                                      , ("name", TextValue " D. A. V. E ")])]]
                       , Return]
  it "parses match clause with any relationship"
    $ "MATCH [a] RETURN"
    `shouldParseQuery` [ Match
                           [ Pattern
                               Nothing
                               [Relationship (AnyRelationship "a") M.empty]]
                       , Return]
  it "parses match clause with any relationship specifying properties"
    $ "MATCH [a { name: ' D. A. V. E ', age: 32, height: 1.6, delta: -10, base: -3.14 }] RETURN"
    `shouldParseQuery` [ Match
                           [ Pattern
                               Nothing
                               [ Relationship
                                   (AnyRelationship "a")
                                   (M.fromList
                                      [ ("age", IntegerValue 32)
                                      , ("base", DoubleValue (-3.14))
                                      , ("delta", IntegerValue (-10))
                                      , ("height", DoubleValue 1.6)
                                      , ("name", TextValue " D. A. V. E ")])]]
                       , Return]
  it "parses match clause with empty relationship"
    $ "MATCH [] RETURN"
    `shouldParseQuery` [ Match
                           [ Pattern
                               Nothing
                               [Relationship EmptyRelationship M.empty]]
                       , Return]
  it "parses match clause with empty relationship specifying properties"
    $ "MATCH [{ name: ' D. A. V. E ', age: 32, height: 1.6, delta: -10, base: -3.14 }] RETURN"
    `shouldParseQuery` [ Match
                           [ Pattern
                               Nothing
                               [ Relationship
                                   EmptyRelationship
                                   (M.fromList
                                      [ ("age", IntegerValue 32)
                                      , ("base", DoubleValue (-3.14))
                                      , ("delta", IntegerValue (-10))
                                      , ("height", DoubleValue 1.6)
                                      , ("name", TextValue " D. A. V. E ")])]]
                       , Return]

runParserMatchDirectionTests = do
  it "parses match clause with right directionality"
    $ "MATCH (p:Person)-[h:HAS]->(c:Car) RETURN"
    `shouldParseQuery` [ Match
                           [ Pattern
                               Nothing
                               [ Node
                                   (LabelledNode (Just "p") ["Person"])
                                   M.empty
                               , ConnectorDirection NoDirection
                               , Relationship
                                   (LabelledRelationship (Just "h") "HAS")
                                   M.empty
                               , ConnectorDirection RightDirection
                               , Node (LabelledNode (Just "c") ["Car"]) M.empty]]
                       , Return]
  it "parses match clause with left directionality"
    $ "MATCH (p:Person)<-[h:HAS]-(c:Car) RETURN"
    `shouldParseQuery` [ Match
                           [ Pattern
                               Nothing
                               [ Node
                                   (LabelledNode (Just "p") ["Person"])
                                   M.empty
                               , ConnectorDirection LeftDirection
                               , Relationship
                                   (LabelledRelationship (Just "h") "HAS")
                                   M.empty
                               , ConnectorDirection NoDirection
                               , Node (LabelledNode (Just "c") ["Car"]) M.empty]]
                       , Return]
  it "parses match clause with no directionality"
    $ "MATCH (p:Person)-[h:HAS]-(c:Car) RETURN"
    `shouldParseQuery` [ Match
                           [ Pattern
                               Nothing
                               [ Node
                                   (LabelledNode (Just "p") ["Person"])
                                   M.empty
                               , ConnectorDirection NoDirection
                               , Relationship
                                   (LabelledRelationship (Just "h") "HAS")
                                   M.empty
                               , ConnectorDirection NoDirection
                               , Node (LabelledNode (Just "c") ["Car"]) M.empty]]
                       , Return]
  it "parses match clause with anonymous right directionality"
    $ "MATCH (p:Person)-->(c:Car) RETURN"
    `shouldParseQuery` [ Match
                           [ Pattern
                               Nothing
                               [ Node
                                   (LabelledNode (Just "p") ["Person"])
                                   M.empty
                               , ConnectorDirection AnonymousRightDirection
                               , Node (LabelledNode (Just "c") ["Car"]) M.empty]]
                       , Return]
  it "parses match clause with anonymous left directionality"
    $ "MATCH (p:Person)<--(c:Car) RETURN"
    `shouldParseQuery` [ Match
                           [ Pattern
                               Nothing
                               [ Node
                                   (LabelledNode (Just "p") ["Person"])
                                   M.empty
                               , ConnectorDirection AnonymousLeftDirection
                               , Node (LabelledNode (Just "c") ["Car"]) M.empty]]
                       , Return]
  it "parses match clause with anonymous no directionality"
    $ "MATCH (p:Person)--(c:Car) RETURN"
    `shouldParseQuery` [ Match
                           [ Pattern
                               Nothing
                               [ Node
                                   (LabelledNode (Just "p") ["Person"])
                                   M.empty
                               , ConnectorDirection AnonymousNoDirection
                               , Node (LabelledNode (Just "c") ["Car"]) M.empty]]
                       , Return]

runParserMatchPatternTests = do
  it "parses match clause with erratic spacing"
    $ "  MATCH  (   : Person{ name: ' D. A. V. E ' , age : 32 , height : 1.6 , delta : -10 , base  : -3.14  } )   -  [  o : OWNS  ] -> (car :Car )   RETURN    "
    `shouldParseQuery` [ Match
                           [ Pattern
                               Nothing
                               [ Node
                                   (LabelledNode Nothing ["Person"])
                                   (M.fromList
                                      [ ("age", IntegerValue 32)
                                      , ("base", DoubleValue (-3.14))
                                      , ("delta", IntegerValue (-10))
                                      , ("height", DoubleValue 1.6)
                                      , ("name", TextValue " D. A. V. E ")])
                               , ConnectorDirection NoDirection
                               , Relationship
                                   (LabelledRelationship (Just "o") "OWNS")
                                   M.empty
                               , ConnectorDirection RightDirection
                               , Node
                                   (LabelledNode (Just "car") ["Car"])
                                   M.empty]]
                       , Return]
  it "parses match clause followed by another match clause"
    $ "MATCH (p:Person)-[:HAS]->(c:Car) MATCH (cat:Cat) RETURN"
    `shouldParseQuery` [ Match
                           [ Pattern
                               Nothing
                               [ Node
                                   (LabelledNode (Just "p") ["Person"])
                                   M.empty
                               , ConnectorDirection NoDirection
                               , Relationship
                                   (LabelledRelationship Nothing "HAS")
                                   M.empty
                               , ConnectorDirection RightDirection
                               , Node (LabelledNode (Just "c") ["Car"]) M.empty]]
                       , Match
                           [ Pattern
                               Nothing
                               [ Node
                                   (LabelledNode (Just "cat") ["Cat"])
                                   M.empty]]
                       , Return]
  it "parses single match clause with multiple patterns"
    $ "MATCH (p:Person), (m:Movie) RETURN"
    `shouldParseQuery` [ Match
                           [ Pattern
                               Nothing
                               [ Node
                                   (LabelledNode (Just "p") ["Person"])
                                   M.empty]
                           , Pattern
                               Nothing
                               [ Node
                                   (LabelledNode (Just "m") ["Movie"])
                                   M.empty]]
                       , Return]
  it "parses match clause with pattern variable"
    $ "MATCH p=(p:Person), q=(m:Movie) MATCH r=(a)--(b) RETURN"
    `shouldParseQuery` [ Match
                           [ Pattern
                               (Just "p")
                               [ Node
                                   (LabelledNode (Just "p") ["Person"])
                                   M.empty]
                           , Pattern
                               (Just "q")
                               [ Node
                                   (LabelledNode (Just "m") ["Movie"])
                                   M.empty]]
                       , Match
                           [ Pattern
                               (Just "r")
                               [ Node (AnyNode "a") M.empty
                               , ConnectorDirection AnonymousNoDirection
                               , Node (AnyNode "b") M.empty]]
                       , Return]

runParserMatchErrorTests = do
  it "fails on invalid clause producing correct error message"
    $ "MARCH"
    `shouldFailQuery` (utoks "MARCH"
                       <> elabel "match clause"
                       <> elabel "optional match clause"
                       <> elabel "return clause"
                       <> eeof)

shouldParseQuery :: Text -> QueryExpr -> Expectation
shouldParseQuery query expectedResult =
  parse parseQuery "" query `shouldParse` expectedResult

shouldFailQuery :: Text -> ET Text -> Expectation
shouldFailQuery query expectedError =
  parse parseQuery "" query `shouldFailWith` err 0 expectedError