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
        context "with relationship hops" runParserMatchRelationshipHopsTests
        context "with direction" runParserMatchDirectionTests
        context "with pattern cases" runParserMatchPatternTests
    context "when parsing optional match query"
      $ do
        runParserOptionalMatchTests

runParserMatchNodeTests = do
  it "parses match clause with node"
    $ "MATCH (per:Person)"
    `shouldParseMatchQuery` Match
      [ Pattern
          Nothing
          Nothing
          [ Node
              (LabelledNode (Just (UnboundText "per")) [UnboundText "Person"])
              M.empty]]
  it "parses match clause with multi labelled node"
    $ "MATCH (per:Person:Actor)"
    `shouldParseMatchQuery` Match
      [ Pattern
          Nothing
          Nothing
          [ Node
              (LabelledNode
                 (Just (UnboundText "per"))
                 [UnboundText "Person", UnboundText "Actor"])
              M.empty]]
  it "parses match clause with labelled node specifying properties"
    $ "MATCH (per:Person { name: ' D. A. V. E ', age: 32, height: 1.6, delta: -10, base: -3.14 })"
    `shouldParseMatchQuery` Match
      [ Pattern
          Nothing
          Nothing
          [ Node
              (LabelledNode (Just (UnboundText "per")) [UnboundText "Person"])
              (M.fromList
                 [ (UnboundText "age", IntegerValue 32)
                 , (UnboundText "base", DoubleValue (-3.14))
                 , (UnboundText "delta", IntegerValue (-10))
                 , (UnboundText "height", DoubleValue 1.6)
                 , (UnboundText "name", TextValue (QuotedText " D. A. V. E "))])]]
  it "parses match clause with anonymous node"
    $ "MATCH (:Person)"
    `shouldParseMatchQuery` Match
      [ Pattern
          Nothing
          Nothing
          [Node (LabelledNode Nothing [UnboundText "Person"]) M.empty]]
  it "parses match clause with anonymous node specifying properties"
    $ "MATCH (:Person { name: ' D. A. V. E ', age: 32, height: 1.6, delta: -10, base: -3.14 })"
    `shouldParseMatchQuery` Match
      [ Pattern
          Nothing
          Nothing
          [ Node
              (LabelledNode Nothing [UnboundText "Person"])
              (M.fromList
                 [ (UnboundText "age", IntegerValue 32)
                 , (UnboundText "base", DoubleValue (-3.14))
                 , (UnboundText "delta", IntegerValue (-10))
                 , (UnboundText "height", DoubleValue 1.6)
                 , (UnboundText "name", TextValue (QuotedText " D. A. V. E "))])]]
  it "parses match clause with empty node"
    $ "MATCH ()"
    `shouldParseMatchQuery` Match
      [Pattern Nothing Nothing [Node EmptyNode M.empty]]
  it "parses match clause with empty node specifying properties"
    $ "MATCH ({ name: ' D. A. V. E ', age: 32, height: 1.6, delta: -10, base: -3.14 })"
    `shouldParseMatchQuery` Match
      [ Pattern
          Nothing
          Nothing
          [ Node
              EmptyNode
              (M.fromList
                 [ (UnboundText "age", IntegerValue 32)
                 , (UnboundText "base", DoubleValue (-3.14))
                 , (UnboundText "delta", IntegerValue (-10))
                 , (UnboundText "height", DoubleValue 1.6)
                 , (UnboundText "name", TextValue (QuotedText " D. A. V. E "))])]]
  it "parses match clause with any node"
    $ "MATCH (n)"
    `shouldParseMatchQuery` Match
      [Pattern Nothing Nothing [Node (AnyNode (UnboundText "n")) M.empty]]
  it "parses match clause with any node specifying properties"
    $ "MATCH (n{ name: ' D. A. V. E ', age: 32, height: 1.6, delta: -10, base: -3.14 })"
    `shouldParseMatchQuery` Match
      [ Pattern
          Nothing
          Nothing
          [ Node
              (AnyNode (UnboundText "n"))
              (M.fromList
                 [ (UnboundText "age", IntegerValue 32)
                 , (UnboundText "base", DoubleValue (-3.14))
                 , (UnboundText "delta", IntegerValue (-10))
                 , (UnboundText "height", DoubleValue 1.6)
                 , (UnboundText "name", TextValue (QuotedText " D. A. V. E "))])]]

runParserOptionalMatchTests = do
  it "parses optional match clause with node"
    $ "OPTIONAL MATCH (per:Person)"
    `shouldParseOptionalMatchQuery` OptionalMatch
      [ Pattern
          Nothing
          Nothing
          [ Node
              (LabelledNode (Just (UnboundText "per")) [UnboundText "Person"])
              M.empty]]
  it "parses match with optional match clause"
    $ "OPTIONAL MATCH (a)-[r:ACTS_IN]->()"
    `shouldParseOptionalMatchQuery` OptionalMatch
      [ Pattern
          Nothing
          Nothing
          [ Node (AnyNode (UnboundText "a")) M.empty
          , ConnectorDirection NoDirection
          , Relationship
              (LabelledRelationship
                 (Just (UnboundText "r"))
                 [UnboundText "ACTS_IN"])
              Nothing
              M.empty
          , ConnectorDirection RightDirection
          , Node EmptyNode M.empty]]

runParserMatchRelationshipTests = do
  it "parses match clause with relationship"
    $ "MATCH [fo:FOLLOWS]"
    `shouldParseMatchQuery` Match
      [ Pattern
          Nothing
          Nothing
          [ Relationship
              (LabelledRelationship
                 (Just (UnboundText "fo"))
                 [UnboundText "FOLLOWS"])
              Nothing
              M.empty]]
  it "parses match clause with variable length relationship"
    $ "MATCH [fo:FOLLOWS*4..6]"
    `shouldParseMatchQuery` Match
      [ Pattern
          Nothing
          Nothing
          [ Relationship
              (LabelledRelationship
                 (Just (UnboundText "fo"))
                 [UnboundText "FOLLOWS"])
              (Just (VariableHops 4 6))
              M.empty]]
  it "parses match clause with fixed length relationship"
    $ "MATCH [fo:FOLLOWS*8]"
    `shouldParseMatchQuery` Match
      [ Pattern
          Nothing
          Nothing
          [ Relationship
              (LabelledRelationship
                 (Just (UnboundText "fo"))
                 [UnboundText "FOLLOWS"])
              (Just (FixedHops 8))
              M.empty]]
  it "parses match clause with multi pipe labelled relationship"
    $ "MATCH [fo:FOLLOWS|UNFOLLOWS]"
    `shouldParseMatchQuery` Match
      [ Pattern
          Nothing
          Nothing
          [ Relationship
              (LabelledRelationship
                 (Just (UnboundText "fo"))
                 [UnboundText "FOLLOWS", UnboundText "UNFOLLOWS"])
              Nothing
              M.empty]]
  it "parses match clause with multi pipe with colon labelled relationship"
    $ "MATCH [fo:FOLLOWS|:UNFOLLOWS]"
    `shouldParseMatchQuery` Match
      [ Pattern
          Nothing
          Nothing
          [ Relationship
              (LabelledRelationship
                 (Just (UnboundText "fo"))
                 [UnboundText "FOLLOWS", UnboundText "UNFOLLOWS"])
              Nothing
              M.empty]]
  it "parses match clause with screaming snake case relationship"
    $ "MATCH [i:IS_A_FAN_OF]"
    `shouldParseMatchQuery` Match
      [ Pattern
          Nothing
          Nothing
          [ Relationship
              (LabelledRelationship
                 (Just (UnboundText "i"))
                 [UnboundText "IS_A_FAN_OF"])
              Nothing
              M.empty]]
  it "parses match clause with relationship specifying properties"
    $ "MATCH [fo:FOLLOWS { name: ' D. A. V. E ', age: 32, height: 1.6, delta: -10, base: -3.14 }]"
    `shouldParseMatchQuery` Match
      [ Pattern
          Nothing
          Nothing
          [ Relationship
              (LabelledRelationship
                 (Just (UnboundText "fo"))
                 [UnboundText "FOLLOWS"])
              Nothing
              (M.fromList
                 [ (UnboundText "age", IntegerValue 32)
                 , (UnboundText "base", DoubleValue (-3.14))
                 , (UnboundText "delta", IntegerValue (-10))
                 , (UnboundText "height", DoubleValue 1.6)
                 , (UnboundText "name", TextValue (QuotedText " D. A. V. E "))])]]
  it "parses match clause with anonymous relationship"
    $ "MATCH [:FOLLOWS]"
    `shouldParseMatchQuery` Match
      [ Pattern
          Nothing
          Nothing
          [ Relationship
              (LabelledRelationship Nothing [UnboundText "FOLLOWS"])
              Nothing
              M.empty]]
  it "parses match clause with anonymous relationship specifying properties"
    $ "MATCH [:FOLLOWS { name: ' D. A. V. E ', age: 32, height: 1.6, delta: -10, base: -3.14 }]"
    `shouldParseMatchQuery` Match
      [ Pattern
          Nothing
          Nothing
          [ Relationship
              (LabelledRelationship Nothing [UnboundText "FOLLOWS"])
              Nothing
              (M.fromList
                 [ (UnboundText "age", IntegerValue 32)
                 , (UnboundText "base", DoubleValue (-3.14))
                 , (UnboundText "delta", IntegerValue (-10))
                 , (UnboundText "height", DoubleValue 1.6)
                 , (UnboundText "name", TextValue (QuotedText " D. A. V. E "))])]]
  it "parses match clause with any relationship"
    $ "MATCH [a]"
    `shouldParseMatchQuery` Match
      [ Pattern
          Nothing
          Nothing
          [Relationship (AnyRelationship (UnboundText "a")) Nothing M.empty]]
  it "parses match clause with any relationship specifying properties"
    $ "MATCH [a { name: ' D. A. V. E ', age: 32, height: 1.6, delta: -10, base: -3.14, today: true, tomorrow: false }]"
    `shouldParseMatchQuery` Match
      [ Pattern
          Nothing
          Nothing
          [ Relationship
              (AnyRelationship (UnboundText "a"))
              Nothing
              (M.fromList
                 [ (UnboundText "age", IntegerValue 32)
                 , (UnboundText "base", DoubleValue (-3.14))
                 , (UnboundText "delta", IntegerValue (-10))
                 , (UnboundText "height", DoubleValue 1.6)
                 , (UnboundText "name", TextValue (QuotedText " D. A. V. E "))
                 , (UnboundText "today", BooleanValue True)
                 , (UnboundText "tomorrow", BooleanValue False)])]]
  it "parses match clause with empty relationship"
    $ "MATCH []"
    `shouldParseMatchQuery` Match
      [ Pattern
          Nothing
          Nothing
          [Relationship EmptyRelationship Nothing M.empty]]
  it "parses match clause with empty relationship specifying properties"
    $ "MATCH [{ name: ' D. A. V. E ', age: 32, height: 1.6, delta: -10, base: -3.14 }]"
    `shouldParseMatchQuery` Match
      [ Pattern
          Nothing
          Nothing
          [ Relationship
              EmptyRelationship
              Nothing
              (M.fromList
                 [ (UnboundText "age", IntegerValue 32)
                 , (UnboundText "base", DoubleValue (-3.14))
                 , (UnboundText "delta", IntegerValue (-10))
                 , (UnboundText "height", DoubleValue 1.6)
                 , (UnboundText "name", TextValue (QuotedText " D. A. V. E "))])]]
  it
    "parses match clause with empty relationship specifying hops and properties"
    $ "MATCH [*1..3{ name: ' D. A. V. E ', age: 32, height: 1.6, delta: -10, base: -3.14 }]"
    `shouldParseMatchQuery` Match
      [ Pattern
          Nothing
          Nothing
          [ Relationship
              EmptyRelationship
              (Just (VariableHops 1 3))
              (M.fromList
                 [ (UnboundText "age", IntegerValue 32)
                 , (UnboundText "base", DoubleValue (-3.14))
                 , (UnboundText "delta", IntegerValue (-10))
                 , (UnboundText "height", DoubleValue 1.6)
                 , (UnboundText "name", TextValue (QuotedText " D. A. V. E "))])]]

runParserMatchRelationshipHopsTests = do
  it "parses match clause with empty relationship and variable length hops"
    $ "MATCH [*2..4]"
    `shouldParseMatchQuery` Match
      [ Pattern
          Nothing
          Nothing
          [Relationship EmptyRelationship (Just (VariableHops 2 4)) M.empty]]
  it "parses match clause with empty relationship and min length hops"
    $ "MATCH [*2..]"
    `shouldParseMatchQuery` Match
      [ Pattern
          Nothing
          Nothing
          [Relationship EmptyRelationship (Just (MinHops 2)) M.empty]]
  it "parses match clause with empty relationship and max length hops"
    $ "MATCH [*..2]"
    `shouldParseMatchQuery` Match
      [ Pattern
          Nothing
          Nothing
          [Relationship EmptyRelationship (Just (MaxHops 2)) M.empty]]
  it "parses match clause with empty relationship and fixed length hops"
    $ "MATCH [*2]"
    `shouldParseMatchQuery` Match
      [ Pattern
          Nothing
          Nothing
          [Relationship EmptyRelationship (Just (FixedHops 2)) M.empty]]
  it "parses match clause with empty relationship and any length hops"
    $ "MATCH [*]"
    `shouldParseMatchQuery` Match
      [ Pattern
          Nothing
          Nothing
          [Relationship EmptyRelationship (Just AnyHops) M.empty]]

runParserMatchDirectionTests = do
  it "parses match clause with right directionality"
    $ "MATCH (p:Person)-[h:HAS]->(c:Car)"
    `shouldParseMatchQuery` Match
      [ Pattern
          Nothing
          Nothing
          [ Node
              (LabelledNode (Just (UnboundText "p")) [UnboundText "Person"])
              M.empty
          , ConnectorDirection NoDirection
          , Relationship
              (LabelledRelationship
                 (Just (UnboundText "h"))
                 [UnboundText "HAS"])
              Nothing
              M.empty
          , ConnectorDirection RightDirection
          , Node
              (LabelledNode (Just (UnboundText "c")) [UnboundText "Car"])
              M.empty]]
  it "parses match clause with left directionality"
    $ "MATCH (p:Person)<-[h:HAS]-(c:Car)"
    `shouldParseMatchQuery` Match
      [ Pattern
          Nothing
          Nothing
          [ Node
              (LabelledNode (Just (UnboundText "p")) [UnboundText "Person"])
              M.empty
          , ConnectorDirection LeftDirection
          , Relationship
              (LabelledRelationship
                 (Just (UnboundText "h"))
                 [UnboundText "HAS"])
              Nothing
              M.empty
          , ConnectorDirection NoDirection
          , Node
              (LabelledNode (Just (UnboundText "c")) [UnboundText "Car"])
              M.empty]]
  it "parses match clause with no directionality"
    $ "MATCH (p:Person)-[h:HAS]-(c:Car)"
    `shouldParseMatchQuery` Match
      [ Pattern
          Nothing
          Nothing
          [ Node
              (LabelledNode (Just (UnboundText "p")) [UnboundText "Person"])
              M.empty
          , ConnectorDirection NoDirection
          , Relationship
              (LabelledRelationship
                 (Just (UnboundText "h"))
                 [UnboundText "HAS"])
              Nothing
              M.empty
          , ConnectorDirection NoDirection
          , Node
              (LabelledNode (Just (UnboundText "c")) [UnboundText "Car"])
              M.empty]]
  it "parses match clause with anonymous right directionality"
    $ "MATCH (p:Person)-->(c:Car)"
    `shouldParseMatchQuery` Match
      [ Pattern
          Nothing
          Nothing
          [ Node
              (LabelledNode (Just (UnboundText "p")) [UnboundText "Person"])
              M.empty
          , ConnectorDirection AnonymousRightDirection
          , Node
              (LabelledNode (Just (UnboundText "c")) [UnboundText "Car"])
              M.empty]]
  it "parses match clause with anonymous left directionality"
    $ "MATCH (p:Person)<--(c:Car)"
    `shouldParseMatchQuery` Match
      [ Pattern
          Nothing
          Nothing
          [ Node
              (LabelledNode (Just (UnboundText "p")) [UnboundText "Person"])
              M.empty
          , ConnectorDirection AnonymousLeftDirection
          , Node
              (LabelledNode (Just (UnboundText "c")) [UnboundText "Car"])
              M.empty]]
  it "parses match clause with anonymous no directionality"
    $ "MATCH (p:Person)--(c:Car)"
    `shouldParseMatchQuery` Match
      [ Pattern
          Nothing
          Nothing
          [ Node
              (LabelledNode (Just (UnboundText "p")) [UnboundText "Person"])
              M.empty
          , ConnectorDirection AnonymousNoDirection
          , Node
              (LabelledNode (Just (UnboundText "c")) [UnboundText "Car"])
              M.empty]]

runParserMatchPatternTests = do
  it "parses match clause with erratic spacing"
    $ "MATCH  (   : Person{ name: ' D. A. V. E ' , age : 32 , height : 1.6 , delta : -10 , base  : -3.14  } )   -  [  o : OWNS  ] -> (car :Car )"
    `shouldParseMatchQuery` Match
      [ Pattern
          Nothing
          Nothing
          [ Node
              (LabelledNode Nothing [UnboundText "Person"])
              (M.fromList
                 [ (UnboundText "age", IntegerValue 32)
                 , (UnboundText "base", DoubleValue (-3.14))
                 , (UnboundText "delta", IntegerValue (-10))
                 , (UnboundText "height", DoubleValue 1.6)
                 , (UnboundText "name", TextValue (QuotedText " D. A. V. E "))])
          , ConnectorDirection NoDirection
          , Relationship
              (LabelledRelationship
                 (Just (UnboundText "o"))
                 [UnboundText "OWNS"])
              Nothing
              M.empty
          , ConnectorDirection RightDirection
          , Node
              (LabelledNode (Just (UnboundText "car")) [UnboundText "Car"])
              M.empty]]
  it "parses single match clause with multiple patterns"
    $ "MATCH (p:Person), (m:Movie)"
    `shouldParseMatchQuery` Match
      [ Pattern
          Nothing
          Nothing
          [ Node
              (LabelledNode (Just (UnboundText "p")) [UnboundText "Person"])
              M.empty]
      , Pattern
          Nothing
          Nothing
          [ Node
              (LabelledNode (Just (UnboundText "m")) [UnboundText "Movie"])
              M.empty]]
  it "parses match clause with pattern variable"
    $ "MATCH p=(p:Person),q=(m:Movie) MATCH r=(a)--(b)"
    `shouldParseMatchQuery` Match
      [ Pattern
          (Just "p")
          Nothing
          [ Node
              (LabelledNode (Just (UnboundText "p")) [UnboundText "Person"])
              M.empty]
      , Pattern
          (Just "q")
          Nothing
          [ Node
              (LabelledNode (Just (UnboundText "m")) [UnboundText "Movie"])
              M.empty]]
  it "parses match clause wrapped in a function"
    $ "MATCH shortestPath((p:Person)-[*]-(j:Job))"
    `shouldParseMatchQuery` Match
      [ Pattern
          Nothing
          (Just "shortestPath")
          [ Node
              (LabelledNode (Just (UnboundText "p")) [UnboundText "Person"])
              M.empty
          , ConnectorDirection NoDirection
          , Relationship EmptyRelationship (Just AnyHops) M.empty
          , ConnectorDirection NoDirection
          , Node
              (LabelledNode (Just (UnboundText "j")) [UnboundText "Job"])
              M.empty]]
  it "parses match clause with pattern variable wrapped in a function"
    $ "MATCH x=shortestPath((p:Person)-[*]-(j:Job))"
    `shouldParseMatchQuery` Match
      [ Pattern
          (Just "x")
          (Just "shortestPath")
          [ Node
              (LabelledNode (Just (UnboundText "p")) [UnboundText "Person"])
              M.empty
          , ConnectorDirection NoDirection
          , Relationship EmptyRelationship (Just AnyHops) M.empty
          , ConnectorDirection NoDirection
          , Node
              (LabelledNode (Just (UnboundText "j")) [UnboundText "Job"])
              M.empty]]
  it "parses strings wrapped in backticks and quotes"
    $ "MATCH (`odd-ch@racter$`:`Spaced Label` {`&property`: 42})-['j':'A Job']"
    `shouldParseMatchQuery` Match
      [ Pattern
          Nothing
          Nothing
          [ Node
              (LabelledNode
                 (Just (BacktickedText "odd-ch@racter$"))
                 [BacktickedText "Spaced Label"])
              (M.fromList [(BacktickedText "&property", IntegerValue 42)])
          , ConnectorDirection NoDirection
          , Relationship
              (LabelledRelationship
                 (Just (QuotedText "j"))
                 [QuotedText "A Job"])
              Nothing
              (M.empty)]]

shouldParseMatchQuery :: Text -> Clause -> Expectation
shouldParseMatchQuery query expectedResult =
  parse parseMatch "" query `shouldParse` expectedResult

shouldParseOptionalMatchQuery :: Text -> Clause -> Expectation
shouldParseOptionalMatchQuery query expectedResult =
  parse parseOptionalMatch "" query `shouldParse` expectedResult