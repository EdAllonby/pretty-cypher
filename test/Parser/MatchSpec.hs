module Parser.MatchSpec where

import           Types
import           Parser.Match
import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec
import           Data.Text as T
import qualified Data.Map as M

runParserMatchTests :: SpecWith ()
runParserMatchTests = describe "Parser.Match"
  $ do
    context "when parsing match query"
      $ do
        context "with node" runParserMatchNodeTests
        context "with relationship" runParserMatchRelationshipTests
        context "with direction" runParserMatchDirectionTests
        context "with pattern cases" runParserMatchPatternTests
    context "when parsing optional match query"
      $ do
        runParserOptionalMatchTests

runParserMatchNodeTests = do
  it "parses match clause with node"
    $ "MATCH (per:Person)"
    `shouldParseMatchQuery` Match
      [Pattern Nothing [Node (LabelledNode (Just "per") ["Person"]) M.empty]]
  it "parses match clause with multi labelled node"
    $ "MATCH (per:Person:Actor)"
    `shouldParseMatchQuery` Match
      [ Pattern
          Nothing
          [Node (LabelledNode (Just "per") ["Person", "Actor"]) M.empty]]
  it "parses match clause with labelled node specifying properties"
    $ "MATCH (per:Person { name: ' D. A. V. E ', age: 32, height: 1.6, delta: -10, base: -3.14 })"
    `shouldParseMatchQuery` Match
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
  it "parses match clause with anonymous node"
    $ "MATCH (:Person)"
    `shouldParseMatchQuery` Match
      [Pattern Nothing [Node (LabelledNode Nothing ["Person"]) M.empty]]
  it "parses match clause with anonymous node specifying properties"
    $ "MATCH (:Person { name: ' D. A. V. E ', age: 32, height: 1.6, delta: -10, base: -3.14 })"
    `shouldParseMatchQuery` Match
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
  it "parses match clause with empty node"
    $ "MATCH ()"
    `shouldParseMatchQuery` Match [Pattern Nothing [Node EmptyNode M.empty]]
  it "parses match clause with empty node specifying properties"
    $ "MATCH ({ name: ' D. A. V. E ', age: 32, height: 1.6, delta: -10, base: -3.14 })"
    `shouldParseMatchQuery` Match
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
  it "parses match clause with any node"
    $ "MATCH (n)"
    `shouldParseMatchQuery` Match
      [Pattern Nothing [Node (AnyNode "n") M.empty]]
  it "parses match clause with any node specifying properties"
    $ "MATCH (n{ name: ' D. A. V. E ', age: 32, height: 1.6, delta: -10, base: -3.14 })"
    `shouldParseMatchQuery` Match
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

runParserOptionalMatchTests = do
  it "parses optional match clause with node"
    $ "OPTIONAL MATCH (per:Person)"
    `shouldParseOptionalMatchQuery` OptionalMatch
      [Pattern Nothing [Node (LabelledNode (Just "per") ["Person"]) M.empty]]
  it "parses match with optional match clause"
    $ "OPTIONAL MATCH (a)-[r:ACTS_IN]->()"
    `shouldParseOptionalMatchQuery` OptionalMatch
      [ Pattern
          Nothing
          [ Node (AnyNode { anyNodeVariable = "a" }) M.empty
          , ConnectorDirection NoDirection
          , Relationship
              (LabelledRelationship { labelledRelationshipVariable = Just "r"
                                    , labelledRelationshipLabel = "ACTS_IN"
                                    })
              M.empty
          , ConnectorDirection RightDirection
          , Node EmptyNode M.empty]]

runParserMatchRelationshipTests = do
  it "parses match clause with relationship"
    $ "MATCH [fo:FOLLOWS]"
    `shouldParseMatchQuery` Match
      [ Pattern
          Nothing
          [Relationship (LabelledRelationship (Just "fo") "FOLLOWS") M.empty]]
  it "parses match clause with screaming snake case relationship"
    $ "MATCH [i:IS_A_FAN_OF]"
    `shouldParseMatchQuery` Match
      [ Pattern
          Nothing
          [ Relationship
              (LabelledRelationship (Just "i") "IS_A_FAN_OF")
              M.empty]]
  it "parses match clause with relationship specifying properties"
    $ "MATCH [fo:FOLLOWS { name: ' D. A. V. E ', age: 32, height: 1.6, delta: -10, base: -3.14 }]"
    `shouldParseMatchQuery` Match
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
  it "parses match clause with anonymous relationship"
    $ "MATCH [:FOLLOWS]"
    `shouldParseMatchQuery` Match
      [ Pattern
          Nothing
          [Relationship (LabelledRelationship Nothing "FOLLOWS") M.empty]]
  it "parses match clause with anonymous relationship specifying properties"
    $ "MATCH [:FOLLOWS { name: ' D. A. V. E ', age: 32, height: 1.6, delta: -10, base: -3.14 }]"
    `shouldParseMatchQuery` Match
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
  it "parses match clause with any relationship"
    $ "MATCH [a]"
    `shouldParseMatchQuery` Match
      [Pattern Nothing [Relationship (AnyRelationship "a") M.empty]]
  it "parses match clause with any relationship specifying properties"
    $ "MATCH [a { name: ' D. A. V. E ', age: 32, height: 1.6, delta: -10, base: -3.14 }]"
    `shouldParseMatchQuery` Match
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
  it "parses match clause with empty relationship"
    $ "MATCH []"
    `shouldParseMatchQuery` Match
      [Pattern Nothing [Relationship EmptyRelationship M.empty]]
  it "parses match clause with empty relationship specifying properties"
    $ "MATCH [{ name: ' D. A. V. E ', age: 32, height: 1.6, delta: -10, base: -3.14 }]"
    `shouldParseMatchQuery` Match
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

runParserMatchDirectionTests = do
  it "parses match clause with right directionality"
    $ "MATCH (p:Person)-[h:HAS]->(c:Car)"
    `shouldParseMatchQuery` Match
      [ Pattern
          Nothing
          [ Node (LabelledNode (Just "p") ["Person"]) M.empty
          , ConnectorDirection NoDirection
          , Relationship (LabelledRelationship (Just "h") "HAS") M.empty
          , ConnectorDirection RightDirection
          , Node (LabelledNode (Just "c") ["Car"]) M.empty]]
  it "parses match clause with left directionality"
    $ "MATCH (p:Person)<-[h:HAS]-(c:Car)"
    `shouldParseMatchQuery` Match
      [ Pattern
          Nothing
          [ Node (LabelledNode (Just "p") ["Person"]) M.empty
          , ConnectorDirection LeftDirection
          , Relationship (LabelledRelationship (Just "h") "HAS") M.empty
          , ConnectorDirection NoDirection
          , Node (LabelledNode (Just "c") ["Car"]) M.empty]]
  it "parses match clause with no directionality"
    $ "MATCH (p:Person)-[h:HAS]-(c:Car)"
    `shouldParseMatchQuery` Match
      [ Pattern
          Nothing
          [ Node (LabelledNode (Just "p") ["Person"]) M.empty
          , ConnectorDirection NoDirection
          , Relationship (LabelledRelationship (Just "h") "HAS") M.empty
          , ConnectorDirection NoDirection
          , Node (LabelledNode (Just "c") ["Car"]) M.empty]]
  it "parses match clause with anonymous right directionality"
    $ "MATCH (p:Person)-->(c:Car)"
    `shouldParseMatchQuery` Match
      [ Pattern
          Nothing
          [ Node (LabelledNode (Just "p") ["Person"]) M.empty
          , ConnectorDirection AnonymousRightDirection
          , Node (LabelledNode (Just "c") ["Car"]) M.empty]]
  it "parses match clause with anonymous left directionality"
    $ "MATCH (p:Person)<--(c:Car)"
    `shouldParseMatchQuery` Match
      [ Pattern
          Nothing
          [ Node (LabelledNode (Just "p") ["Person"]) M.empty
          , ConnectorDirection AnonymousLeftDirection
          , Node (LabelledNode (Just "c") ["Car"]) M.empty]]
  it "parses match clause with anonymous no directionality"
    $ "MATCH (p:Person)--(c:Car)"
    `shouldParseMatchQuery` Match
      [ Pattern
          Nothing
          [ Node (LabelledNode (Just "p") ["Person"]) M.empty
          , ConnectorDirection AnonymousNoDirection
          , Node (LabelledNode (Just "c") ["Car"]) M.empty]]

runParserMatchPatternTests = do
  it "parses match clause with erratic spacing"
    $ "MATCH  (   : Person{ name: ' D. A. V. E ' , age : 32 , height : 1.6 , delta : -10 , base  : -3.14  } )   -  [  o : OWNS  ] -> (car :Car )"
    `shouldParseMatchQuery` Match
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
          , Relationship (LabelledRelationship (Just "o") "OWNS") M.empty
          , ConnectorDirection RightDirection
          , Node (LabelledNode (Just "car") ["Car"]) M.empty]]
  it "parses single match clause with multiple patterns"
    $ "MATCH (p:Person), (m:Movie)"
    `shouldParseMatchQuery` Match
      [ Pattern Nothing [Node (LabelledNode (Just "p") ["Person"]) M.empty]
      , Pattern Nothing [Node (LabelledNode (Just "m") ["Movie"]) M.empty]]
  it "parses match clause with pattern variable"
    $ "MATCH p=(p:Person),q=(m:Movie) MATCH r=(a)--(b)"
    `shouldParseMatchQuery` Match
      [ Pattern (Just "p") [Node (LabelledNode (Just "p") ["Person"]) M.empty]
      , Pattern (Just "q") [Node (LabelledNode (Just "m") ["Movie"]) M.empty]]

shouldParseMatchQuery :: Text -> Clause -> Expectation
shouldParseMatchQuery query expectedResult =
  parse parseMatch "" query `shouldParse` expectedResult

shouldParseOptionalMatchQuery :: Text -> Clause -> Expectation
shouldParseOptionalMatchQuery query expectedResult =
  parse parseOptionalMatch "" query `shouldParse` expectedResult