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
    `shouldParsePatternQuery` [ Node
                                  (LabelledPatternComponentType
                                     (Just (UnboundText "per"))
                                     [UnboundText "Person"])
                                  M.empty]
  it "parses pattern with multi labelled node"
    $ "(per:Person:Actor)"
    `shouldParsePatternQuery` [ Node
                                  (LabelledPatternComponentType
                                     (Just (UnboundText "per"))
                                     [ UnboundText "Person"
                                     , UnboundText "Actor"])
                                  M.empty]
  it "parses pattern with labelled node specifying properties"
    $ "(per:Person { name: ' D. A. V. E ', age: 32, height: 1.6, delta: -10, base: -3.14, today: true, tomorrow: false })"
    `shouldParsePatternQuery` [ Node
                                  (LabelledPatternComponentType
                                     (Just (UnboundText "per"))
                                     [UnboundText "Person"])
                                  standardProperties]
  it "parses pattern with anonymous node"
    $ "(:Person)"
    `shouldParsePatternQuery` [ Node
                                  (LabelledPatternComponentType
                                     Nothing
                                     [UnboundText "Person"])
                                  M.empty]
  it "parses pattern with anonymous node specifying properties"
    $ "(:Person { name: ' D. A. V. E ', age: 32, height: 1.6, delta: -10, base: -3.14, today: true, tomorrow: false })"
    `shouldParsePatternQuery` [ Node
                                  (LabelledPatternComponentType
                                     Nothing
                                     [UnboundText "Person"])
                                  standardProperties]
  it "parses pattern with empty node"
    $ "()" `shouldParsePatternQuery` [Node EmptyPatternComponentType M.empty]
  it "parses pattern with empty node specifying properties"
    $ "({ name: ' D. A. V. E ', age: 32, height: 1.6, delta: -10, base: -3.14, today: true, tomorrow: false  })"
    `shouldParsePatternQuery` [ Node
                                  EmptyPatternComponentType
                                  standardProperties]
  it "parses pattern with any node"
    $ "(n)"
    `shouldParsePatternQuery` [ Node
                                  (AnyPatternComponentType (UnboundText "n"))
                                  M.empty]
  it "parses pattern with any node specifying properties"
    $ "(n{ name: ' D. A. V. E ', age: 32, height: 1.6, delta: -10, base: -3.14, today: true, tomorrow: false })"
    `shouldParsePatternQuery` [ Node
                                  (AnyPatternComponentType (UnboundText "n"))
                                  standardProperties]

runParserPatternRelationshipTests :: Spec
runParserPatternRelationshipTests = do
  it "parses pattern with relationship"
    $ "[fo:FOLLOWS]"
    `shouldParsePatternQuery` [ Relationship
                                  (LabelledPatternComponentType
                                     (Just (UnboundText "fo"))
                                     [UnboundText "FOLLOWS"])
                                  Nothing
                                  M.empty]
  it "parses pattern with variable length relationship"
    $ "[fo:FOLLOWS*4..6]"
    `shouldParsePatternQuery` [ Relationship
                                  (LabelledPatternComponentType
                                     (Just (UnboundText "fo"))
                                     [UnboundText "FOLLOWS"])
                                  (Just (VariableHops 4 6))
                                  M.empty]
  it "parses pattern with fixed length relationship"
    $ "[fo:FOLLOWS*8]"
    `shouldParsePatternQuery` [ Relationship
                                  (LabelledPatternComponentType
                                     (Just (UnboundText "fo"))
                                     [UnboundText "FOLLOWS"])
                                  (Just (FixedHops 8))
                                  M.empty]
  it "parses pattern with multi pipe labelled relationship"
    $ "[fo:FOLLOWS|UNFOLLOWS]"
    `shouldParsePatternQuery` [ Relationship
                                  (LabelledPatternComponentType
                                     (Just (UnboundText "fo"))
                                     [ UnboundText "FOLLOWS"
                                     , UnboundText "UNFOLLOWS"])
                                  Nothing
                                  M.empty]
  it "parses pattern with multi pipe with colon labelled relationship"
    $ "[fo:FOLLOWS|:UNFOLLOWS]"
    `shouldParsePatternQuery` [ Relationship
                                  (LabelledPatternComponentType
                                     (Just (UnboundText "fo"))
                                     [ UnboundText "FOLLOWS"
                                     , UnboundText "UNFOLLOWS"])
                                  Nothing
                                  M.empty]
  it "parses pattern with screaming snake case relationship"
    $ "[i:IS_A_FAN_OF]"
    `shouldParsePatternQuery` [ Relationship
                                  (LabelledPatternComponentType
                                     (Just (UnboundText "i"))
                                     [UnboundText "IS_A_FAN_OF"])
                                  Nothing
                                  M.empty]
  it "parses pattern with relationship specifying properties"
    $ "[fo:FOLLOWS { name: ' D. A. V. E ', age: 32, height: 1.6, delta: -10, base: -3.14, today: true, tomorrow: false }]"
    `shouldParsePatternQuery` [ Relationship
                                  (LabelledPatternComponentType
                                     (Just (UnboundText "fo"))
                                     [UnboundText "FOLLOWS"])
                                  Nothing
                                  standardProperties]
  it "parses pattern with anonymous relationship"
    $ "[:FOLLOWS]"
    `shouldParsePatternQuery` [ Relationship
                                  (LabelledPatternComponentType
                                     Nothing
                                     [UnboundText "FOLLOWS"])
                                  Nothing
                                  M.empty]
  it "parses pattern with anonymous relationship specifying properties"
    $ "[:FOLLOWS { name: ' D. A. V. E ', age: 32, height: 1.6, delta: -10, base: -3.14, today: true, tomorrow: false }]"
    `shouldParsePatternQuery` [ Relationship
                                  (LabelledPatternComponentType
                                     Nothing
                                     [UnboundText "FOLLOWS"])
                                  Nothing
                                  standardProperties]
  it "parses pattern with any relationship"
    $ "[a]"
    `shouldParsePatternQuery` [ Relationship
                                  (AnyPatternComponentType (UnboundText "a"))
                                  Nothing
                                  M.empty]
  it "parses pattern with any relationship specifying properties"
    $ "[a { name: ' D. A. V. E ', age: 32, height: 1.6, delta: -10, base: -3.14, today: true, tomorrow: false }]"
    `shouldParsePatternQuery` [ Relationship
                                  (AnyPatternComponentType (UnboundText "a"))
                                  Nothing
                                  standardProperties]
  it "parses pattern with empty relationship"
    $ "[]"
    `shouldParsePatternQuery` [ Relationship
                                  EmptyPatternComponentType
                                  Nothing
                                  M.empty]
  it "parses pattern with empty relationship specifying properties"
    $ "[{ name: ' D. A. V. E ', age: 32, height: 1.6, delta: -10, base: -3.14, today: true, tomorrow: false }]"
    `shouldParsePatternQuery` [ Relationship
                                  EmptyPatternComponentType
                                  Nothing
                                  standardProperties]
  it "parses pattern with empty relationship specifying hops and properties"
    $ "[*1..3{ name: ' D. A. V. E ', age: 32, height: 1.6, delta: -10, base: -3.14, today: true, tomorrow: false }]"
    `shouldParsePatternQuery` [ Relationship
                                  EmptyPatternComponentType
                                  (Just (VariableHops 1 3))
                                  standardProperties]

runParserPatternRelationshipHopsTests :: Spec
runParserPatternRelationshipHopsTests = do
  it "parses pattern with empty relationship and variable length hops"
    $ "[*2..4]"
    `shouldParsePatternQuery` [ Relationship
                                  EmptyPatternComponentType
                                  (Just (VariableHops 2 4))
                                  M.empty]
  it "parses pattern with empty relationship and min length hops"
    $ "[*2..]"
    `shouldParsePatternQuery` [ Relationship
                                  EmptyPatternComponentType
                                  (Just (MinHops 2))
                                  M.empty]
  it "parses pattern with empty relationship and max length hops"
    $ "[*..2]"
    `shouldParsePatternQuery` [ Relationship
                                  EmptyPatternComponentType
                                  (Just (MaxHops 2))
                                  M.empty]
  it "parses pattern with empty relationship and fixed length hops"
    $ "[*2]"
    `shouldParsePatternQuery` [ Relationship
                                  EmptyPatternComponentType
                                  (Just (FixedHops 2))
                                  M.empty]
  it "parses pattern with empty relationship and any length hops"
    $ "[*]"
    `shouldParsePatternQuery` [ Relationship
                                  EmptyPatternComponentType
                                  (Just AnyHops)
                                  M.empty]

runParserPatternDirectionTests :: Spec
runParserPatternDirectionTests = do
  it "parses pattern with right directionality"
    $ "(p:Person)-[h:HAS]->(c:Car)"
    `shouldParsePatternQuery` [ Node personLabelledNode M.empty
                              , ConnectorDirection NoDirection
                              , Relationship
                                  hasLabelledRelationship
                                  Nothing
                                  M.empty
                              , ConnectorDirection RightDirection
                              , Node carLabelledNode M.empty]
  it "parses pattern with left directionality"
    $ "(p:Person)<-[h:HAS]-(c:Car)"
    `shouldParsePatternQuery` [ Node personLabelledNode M.empty
                              , ConnectorDirection LeftDirection
                              , Relationship
                                  hasLabelledRelationship
                                  Nothing
                                  M.empty
                              , ConnectorDirection NoDirection
                              , Node carLabelledNode M.empty]
  it "parses pattern with no directionality"
    $ "(p:Person)-[h:HAS]-(c:Car)"
    `shouldParsePatternQuery` [ Node personLabelledNode M.empty
                              , ConnectorDirection NoDirection
                              , Relationship
                                  hasLabelledRelationship
                                  Nothing
                                  M.empty
                              , ConnectorDirection NoDirection
                              , Node carLabelledNode M.empty]
  it "parses pattern with anonymous right directionality"
    $ "(p:Person)-->(c:Car)"
    `shouldParsePatternQuery` [ Node personLabelledNode M.empty
                              , ConnectorDirection AnonymousRightDirection
                              , Node carLabelledNode M.empty]
  it "parses pattern with anonymous left directionality"
    $ "(p:Person)<--(c:Car)"
    `shouldParsePatternQuery` [ Node personLabelledNode M.empty
                              , ConnectorDirection AnonymousLeftDirection
                              , Node carLabelledNode M.empty]
  it "parses pattern with anonymous no directionality"
    $ "(p:Person)--(c:Car)"
    `shouldParsePatternQuery` [ Node personLabelledNode M.empty
                              , ConnectorDirection AnonymousNoDirection
                              , Node carLabelledNode M.empty]

runParserPatternPatternTests :: Spec
runParserPatternPatternTests = do
  it "parses pattern with erratic spacing"
    $ "(   : Person{ name: ' D. A. V. E ' , age : 32 , height : 1.6 , delta : -10 , base  : -3.14 , today: true, tomorrow: false } )   -  [  h : HAS  ] -> (car :Car )"
    `shouldParsePatternQuery` [ Node
                                  (LabelledPatternComponentType
                                     Nothing
                                     [UnboundText "Person"])
                                  standardProperties
                              , ConnectorDirection NoDirection
                              , Relationship
                                  hasLabelledRelationship
                                  Nothing
                                  M.empty
                              , ConnectorDirection RightDirection
                              , Node
                                  (LabelledPatternComponentType
                                     (Just (UnboundText "car"))
                                     [UnboundText "Car"])
                                  M.empty]
  it "parses strings wrapped in backticks and quotes"
    $ "(`odd-ch@racter$`:`Spaced Label` {`&property`: 42})-['j':'A Job']"
    `shouldParsePatternQuery` [ Node
                                  (LabelledPatternComponentType
                                     (Just (BacktickedText "odd-ch@racter$"))
                                     [BacktickedText "Spaced Label"])
                                  (M.fromList
                                     [ ( BacktickedText "&property"
                                         , IntegerValue 42)])
                              , ConnectorDirection NoDirection
                              , Relationship
                                  (LabelledPatternComponentType
                                     (Just (QuotedText "j"))
                                     [QuotedText "A Job"])
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

personLabelledNode :: PatternComponentType
personLabelledNode =
  LabelledPatternComponentType (Just (UnboundText "p")) [UnboundText "Person"]

carLabelledNode :: PatternComponentType
carLabelledNode =
  LabelledPatternComponentType (Just (UnboundText "c")) [UnboundText "Car"]

hasLabelledRelationship :: PatternComponentType
hasLabelledRelationship =
  LabelledPatternComponentType (Just (UnboundText "h")) [UnboundText "HAS"]