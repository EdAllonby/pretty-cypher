module Cypher.Parser.PatternSpec (runParserPatternTests) where

import           Cypher.Types
import           Cypher.Parser.Pattern (parsePattern)
import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec
import           Data.Text as T
import qualified Data.Map as M

runParserPatternTests :: SpecWith ()
runParserPatternTests = describe "Cypher.Parser.Pattern"
  $ do
    context "when parsing pattern"
      $ do
        context "with node" runParserPatternNodeTests
        context "with relationship" runParserPatternRelationshipTests
        context "with relationship hops" runParserPatternRelationshipHopsTests
        context "with direction" runParserPatternDirectionTests
        context "with pattern cases" runParserPatternPatternTests

runParserPatternNodeTests :: Spec
runParserPatternNodeTests = do
  it "parses pattern with node"
    $ "(per:Person)"
    `shouldParsePatternQuery` Pattern
      Nothing
      Nothing
      [ Node
          (LabelledNode (Just (UnboundText "per")) [UnboundText "Person"])
          M.empty]
  it "parses pattern with multi labelled node"
    $ "(per:Person:Actor)"
    `shouldParsePatternQuery` Pattern
      Nothing
      Nothing
      [ Node
          (LabelledNode
             (Just (UnboundText "per"))
             [UnboundText "Person", UnboundText "Actor"])
          M.empty]
  it "parses pattern with labelled node specifying properties"
    $ "(per:Person { name: ' D. A. V. E ', age: 32, height: 1.6, delta: -10, base: -3.14, today: true, tomorrow: false })"
    `shouldParsePatternQuery` Pattern
      Nothing
      Nothing
      [ Node
          (LabelledNode (Just (UnboundText "per")) [UnboundText "Person"])
          standardProperties]
  it "parses pattern with anonymous node"
    $ "(:Person)"
    `shouldParsePatternQuery` Pattern
      Nothing
      Nothing
      [Node (LabelledNode Nothing [UnboundText "Person"]) M.empty]
  it "parses pattern with anonymous node specifying properties"
    $ "(:Person { name: ' D. A. V. E ', age: 32, height: 1.6, delta: -10, base: -3.14, today: true, tomorrow: false })"
    `shouldParsePatternQuery` Pattern
      Nothing
      Nothing
      [Node (LabelledNode Nothing [UnboundText "Person"]) standardProperties]
  it "parses pattern with empty node"
    $ "()"
    `shouldParsePatternQuery` Pattern Nothing Nothing [Node EmptyNode M.empty]
  it "parses pattern with empty node specifying properties"
    $ "({ name: ' D. A. V. E ', age: 32, height: 1.6, delta: -10, base: -3.14, today: true, tomorrow: false  })"
    `shouldParsePatternQuery` Pattern
      Nothing
      Nothing
      [Node EmptyNode standardProperties]
  it "parses pattern with any node"
    $ "(n)"
    `shouldParsePatternQuery` Pattern
      Nothing
      Nothing
      [Node (AnyNode (UnboundText "n")) M.empty]
  it "parses pattern with any node specifying properties"
    $ "(n{ name: ' D. A. V. E ', age: 32, height: 1.6, delta: -10, base: -3.14, today: true, tomorrow: false })"
    `shouldParsePatternQuery` Pattern
      Nothing
      Nothing
      [Node (AnyNode (UnboundText "n")) standardProperties]

runParserPatternRelationshipTests :: Spec
runParserPatternRelationshipTests = do
  it "parses pattern with relationship"
    $ "[fo:FOLLOWS]"
    `shouldParsePatternQuery` Pattern
      Nothing
      Nothing
      [ Relationship
          (LabelledRelationship
             (Just (UnboundText "fo"))
             [UnboundText "FOLLOWS"])
          Nothing
          M.empty]
  it "parses pattern with variable length relationship"
    $ "[fo:FOLLOWS*4..6]"
    `shouldParsePatternQuery` Pattern
      Nothing
      Nothing
      [ Relationship
          (LabelledRelationship
             (Just (UnboundText "fo"))
             [UnboundText "FOLLOWS"])
          (Just (VariableHops 4 6))
          M.empty]
  it "parses pattern with fixed length relationship"
    $ "[fo:FOLLOWS*8]"
    `shouldParsePatternQuery` Pattern
      Nothing
      Nothing
      [ Relationship
          (LabelledRelationship
             (Just (UnboundText "fo"))
             [UnboundText "FOLLOWS"])
          (Just (FixedHops 8))
          M.empty]
  it "parses pattern with multi pipe labelled relationship"
    $ "[fo:FOLLOWS|UNFOLLOWS]"
    `shouldParsePatternQuery` Pattern
      Nothing
      Nothing
      [ Relationship
          (LabelledRelationship
             (Just (UnboundText "fo"))
             [UnboundText "FOLLOWS", UnboundText "UNFOLLOWS"])
          Nothing
          M.empty]
  it "parses pattern with multi pipe with colon labelled relationship"
    $ "[fo:FOLLOWS|:UNFOLLOWS]"
    `shouldParsePatternQuery` Pattern
      Nothing
      Nothing
      [ Relationship
          (LabelledRelationship
             (Just (UnboundText "fo"))
             [UnboundText "FOLLOWS", UnboundText "UNFOLLOWS"])
          Nothing
          M.empty]
  it "parses pattern with screaming snake case relationship"
    $ "[i:IS_A_FAN_OF]"
    `shouldParsePatternQuery` Pattern
      Nothing
      Nothing
      [ Relationship
          (LabelledRelationship
             (Just (UnboundText "i"))
             [UnboundText "IS_A_FAN_OF"])
          Nothing
          M.empty]
  it "parses pattern with relationship specifying properties"
    $ "[fo:FOLLOWS { name: ' D. A. V. E ', age: 32, height: 1.6, delta: -10, base: -3.14, today: true, tomorrow: false }]"
    `shouldParsePatternQuery` Pattern
      Nothing
      Nothing
      [ Relationship
          (LabelledRelationship
             (Just (UnboundText "fo"))
             [UnboundText "FOLLOWS"])
          Nothing
          standardProperties]
  it "parses pattern with anonymous relationship"
    $ "[:FOLLOWS]"
    `shouldParsePatternQuery` Pattern
      Nothing
      Nothing
      [ Relationship
          (LabelledRelationship Nothing [UnboundText "FOLLOWS"])
          Nothing
          M.empty]
  it "parses pattern with anonymous relationship specifying properties"
    $ "[:FOLLOWS { name: ' D. A. V. E ', age: 32, height: 1.6, delta: -10, base: -3.14, today: true, tomorrow: false }]"
    `shouldParsePatternQuery` Pattern
      Nothing
      Nothing
      [ Relationship
          (LabelledRelationship Nothing [UnboundText "FOLLOWS"])
          Nothing
          standardProperties]
  it "parses pattern with any relationship"
    $ "[a]"
    `shouldParsePatternQuery` Pattern
      Nothing
      Nothing
      [Relationship (AnyRelationship (UnboundText "a")) Nothing M.empty]
  it "parses pattern with any relationship specifying properties"
    $ "[a { name: ' D. A. V. E ', age: 32, height: 1.6, delta: -10, base: -3.14, today: true, tomorrow: false }]"
    `shouldParsePatternQuery` Pattern
      Nothing
      Nothing
      [ Relationship
          (AnyRelationship (UnboundText "a"))
          Nothing
          standardProperties]
  it "parses pattern with empty relationship"
    $ "[]"
    `shouldParsePatternQuery` Pattern
      Nothing
      Nothing
      [Relationship EmptyRelationship Nothing M.empty]
  it "parses pattern with empty relationship specifying properties"
    $ "[{ name: ' D. A. V. E ', age: 32, height: 1.6, delta: -10, base: -3.14, today: true, tomorrow: false }]"
    `shouldParsePatternQuery` Pattern
      Nothing
      Nothing
      [Relationship EmptyRelationship Nothing standardProperties]
  it "parses pattern with empty relationship specifying hops and properties"
    $ "[*1..3{ name: ' D. A. V. E ', age: 32, height: 1.6, delta: -10, base: -3.14, today: true, tomorrow: false }]"
    `shouldParsePatternQuery` Pattern
      Nothing
      Nothing
      [ Relationship
          EmptyRelationship
          (Just (VariableHops 1 3))
          standardProperties]

runParserPatternRelationshipHopsTests :: Spec
runParserPatternRelationshipHopsTests = do
  it "parses pattern with empty relationship and variable length hops"
    $ "[*2..4]"
    `shouldParsePatternQuery` Pattern
      Nothing
      Nothing
      [Relationship EmptyRelationship (Just (VariableHops 2 4)) M.empty]
  it "parses pattern with empty relationship and min length hops"
    $ "[*2..]"
    `shouldParsePatternQuery` Pattern
      Nothing
      Nothing
      [Relationship EmptyRelationship (Just (MinHops 2)) M.empty]
  it "parses pattern with empty relationship and max length hops"
    $ "[*..2]"
    `shouldParsePatternQuery` Pattern
      Nothing
      Nothing
      [Relationship EmptyRelationship (Just (MaxHops 2)) M.empty]
  it "parses pattern with empty relationship and fixed length hops"
    $ "[*2]"
    `shouldParsePatternQuery` Pattern
      Nothing
      Nothing
      [Relationship EmptyRelationship (Just (FixedHops 2)) M.empty]
  it "parses pattern with empty relationship and any length hops"
    $ "[*]"
    `shouldParsePatternQuery` Pattern
      Nothing
      Nothing
      [Relationship EmptyRelationship (Just AnyHops) M.empty]

runParserPatternDirectionTests :: Spec
runParserPatternDirectionTests = do
  it "parses pattern with right directionality"
    $ "(p:Person)-[h:HAS]->(c:Car)"
    `shouldParsePatternQuery` Pattern
      Nothing
      Nothing
      [ Node personLabelledNode M.empty
      , ConnectorDirection NoDirection
      , Relationship hasLabelledRelationship Nothing M.empty
      , ConnectorDirection RightDirection
      , Node carLabelledNode M.empty]
  it "parses pattern with left directionality"
    $ "(p:Person)<-[h:HAS]-(c:Car)"
    `shouldParsePatternQuery` Pattern
      Nothing
      Nothing
      [ Node personLabelledNode M.empty
      , ConnectorDirection LeftDirection
      , Relationship hasLabelledRelationship Nothing M.empty
      , ConnectorDirection NoDirection
      , Node carLabelledNode M.empty]
  it "parses pattern with no directionality"
    $ "(p:Person)-[h:HAS]-(c:Car)"
    `shouldParsePatternQuery` Pattern
      Nothing
      Nothing
      [ Node personLabelledNode M.empty
      , ConnectorDirection NoDirection
      , Relationship hasLabelledRelationship Nothing M.empty
      , ConnectorDirection NoDirection
      , Node carLabelledNode M.empty]
  it "parses pattern with anonymous right directionality"
    $ "(p:Person)-->(c:Car)"
    `shouldParsePatternQuery` Pattern
      Nothing
      Nothing
      [ Node personLabelledNode M.empty
      , ConnectorDirection AnonymousRightDirection
      , Node carLabelledNode M.empty]
  it "parses pattern with anonymous left directionality"
    $ "(p:Person)<--(c:Car)"
    `shouldParsePatternQuery` Pattern
      Nothing
      Nothing
      [ Node personLabelledNode M.empty
      , ConnectorDirection AnonymousLeftDirection
      , Node carLabelledNode M.empty]
  it "parses pattern with anonymous no directionality"
    $ "(p:Person)--(c:Car)"
    `shouldParsePatternQuery` Pattern
      Nothing
      Nothing
      [ Node personLabelledNode M.empty
      , ConnectorDirection AnonymousNoDirection
      , Node carLabelledNode M.empty]

runParserPatternPatternTests :: Spec
runParserPatternPatternTests = do
  it "parses pattern with erratic spacing"
    $ "(   : Person{ name: ' D. A. V. E ' , age : 32 , height : 1.6 , delta : -10 , base  : -3.14 , today: true, tomorrow: false } )   -  [  h : HAS  ] -> (car :Car )"
    `shouldParsePatternQuery` Pattern
      Nothing
      Nothing
      [ Node (LabelledNode Nothing [UnboundText "Person"]) standardProperties
      , ConnectorDirection NoDirection
      , Relationship hasLabelledRelationship Nothing M.empty
      , ConnectorDirection RightDirection
      , Node
          (LabelledNode (Just (UnboundText "car")) [UnboundText "Car"])
          M.empty]
  it "parses pattern clause wrapped in a function"
    $ "shortestPath((p:Person)-[*]-(j:Job))"
    `shouldParsePatternQuery` Pattern
      Nothing
      (Just "shortestPath")
      [ Node personLabelledNode M.empty
      , ConnectorDirection NoDirection
      , Relationship EmptyRelationship (Just AnyHops) M.empty
      , ConnectorDirection NoDirection
      , Node
          (LabelledNode (Just (UnboundText "j")) [UnboundText "Job"])
          M.empty]
  it "parses pattern with pattern variable wrapped in a function"
    $ "x=shortestPath((p:Person)-[*]-(c:Car))"
    `shouldParsePatternQuery` Pattern
      (Just "x")
      (Just "shortestPath")
      [ Node personLabelledNode M.empty
      , ConnectorDirection NoDirection
      , Relationship EmptyRelationship (Just AnyHops) M.empty
      , ConnectorDirection NoDirection
      , Node carLabelledNode M.empty]
  it "parses strings wrapped in backticks and quotes"
    $ "(`odd-ch@racter$`:`Spaced Label` {`&property`: 42})-['j':'A Job']"
    `shouldParsePatternQuery` Pattern
      Nothing
      Nothing
      [ Node
          (LabelledNode
             (Just (BacktickedText "odd-ch@racter$"))
             [BacktickedText "Spaced Label"])
          (M.fromList [(BacktickedText "&property", IntegerValue 42)])
      , ConnectorDirection NoDirection
      , Relationship
          (LabelledRelationship (Just (QuotedText "j")) [QuotedText "A Job"])
          Nothing
          M.empty]

shouldParsePatternQuery :: Text -> Pattern -> Expectation
shouldParsePatternQuery query expectedResult =
  parse parsePattern "" query `shouldParse` expectedResult

standardProperties :: M.Map LiteralText PropertyValue
standardProperties = M.fromList
  [ (UnboundText "age", IntegerValue 32)
  , (UnboundText "base", DoubleValue (-3.14))
  , (UnboundText "delta", IntegerValue (-10))
  , (UnboundText "height", DoubleValue 1.6)
  , (UnboundText "name", TextValue (QuotedText " D. A. V. E "))
  , (UnboundText "today", BooleanValue True)
  , (UnboundText "tomorrow", BooleanValue False)]

personLabelledNode :: NodeType
personLabelledNode =
  LabelledNode (Just (UnboundText "p")) [UnboundText "Person"]

carLabelledNode :: NodeType
carLabelledNode = LabelledNode (Just (UnboundText "c")) [UnboundText "Car"]

hasLabelledRelationship :: RelationshipType
hasLabelledRelationship =
  LabelledRelationship (Just (UnboundText "h")) [UnboundText "HAS"]