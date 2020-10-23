module ParserSpec where

import           Types
import           Parser
import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec
import           Data.Text as T
import qualified Data.Map as M

runParserTests :: SpecWith ()
runParserTests = describe "Parser"
  $ do
    context "when parsing match only query"
      $ do
        context "with node" runParserMatchNodeTests
        context "with relationship" runParserMatchRelationshipTests
        context "with direction" runParserMatchDirectionTests
        context "with odd cases" runParserMatchOddTests

runParserMatchNodeTests = do
  it "parses match clause with node"
    $ "MATCH (per:Person) RETURN"
    `shouldParseQuery` Match
      [Node $ LabelledNode (Just "per") "Person" M.empty]
      Return
  it "parses match clause with node specifying properties"
    $ "MATCH (per:Person { name: ' D. A. V. E ', age: 32, height: 1.6, delta: -10, base: -3.14 }) RETURN"
    `shouldParseQuery` Match
      [ Node
          $ LabelledNode
            (Just "per")
            "Person"
            (M.fromList
               [ ("age", IntegerValue 32)
               , ("base", DoubleValue (-3.14))
               , ("delta", IntegerValue (-10))
               , ("height", DoubleValue 1.6)
               , ("name", TextValue " D. A. V. E ")])]
      Return
  it "parses match clause with anonymous node"
    $ "MATCH (:Person) RETURN"
    `shouldParseQuery` Match
      [Node $ LabelledNode Nothing "Person" M.empty]
      Return
  it "parses match clause with empty node"
    $ "MATCH () RETURN" `shouldParseQuery` Match [Node EmptyNode] Return
  it "parses match clause with any node"
    $ "MATCH (n) RETURN" `shouldParseQuery` Match [Node $ AnyNode "n"] Return

runParserMatchRelationshipTests = do
  it "parses match clause with relationship"
    $ "MATCH [fo:FOLLOWS] RETURN"
    `shouldParseQuery` Match
      [Relationship $ LabelledRelationship (Just "fo") "FOLLOWS" M.empty]
      Return
  it "parses match clause with relationship specifying properties"
    $ "MATCH [fo:FOLLOWS { name: ' D. A. V. E ', age: 32, height: 1.6, delta: -10, base: -3.14 }] RETURN"
    `shouldParseQuery` Match
      [ Relationship
          $ LabelledRelationship
            (Just "fo")
            "FOLLOWS"
            (M.fromList
               [ ("age", IntegerValue 32)
               , ("base", DoubleValue (-3.14))
               , ("delta", IntegerValue (-10))
               , ("height", DoubleValue 1.6)
               , ("name", TextValue " D. A. V. E ")])]
      Return
  it "parses match clause with anonymous relationship"
    $ "MATCH [:FOLLOWS] RETURN"
    `shouldParseQuery` Match
      [Relationship $ LabelledRelationship Nothing "FOLLOWS" M.empty]
      Return
  it "parses match clause with any relationship"
    $ "MATCH [a] RETURN"
    `shouldParseQuery` Match [Relationship $ AnyRelationship "a"] Return

runParserMatchDirectionTests = do
  it "parses match clause with right directionality"
    $ "MATCH (p:Person)-[h:HAS]->(c:Car) RETURN"
    `shouldParseQuery` Match
      [ Node $ LabelledNode (Just "p") "Person" M.empty
      , ConnectorDirection NoDirection
      , Relationship $ LabelledRelationship (Just "h") "HAS" M.empty
      , ConnectorDirection RightDirection
      , Node $ LabelledNode (Just "c") "Car" M.empty]
      Return
  it "parses match clause with left directionality"
    $ "MATCH (p:Person)<-[h:HAS]-(c:Car) RETURN"
    `shouldParseQuery` Match
      [ Node $ LabelledNode (Just "p") "Person" M.empty
      , ConnectorDirection LeftDirection
      , Relationship $ LabelledRelationship (Just "h") "HAS" M.empty
      , ConnectorDirection NoDirection
      , Node $ LabelledNode (Just "c") "Car" M.empty]
      Return
  it "parses match clause with no directionality"
    $ "MATCH (p:Person)-[h:HAS]-(c:Car) RETURN"
    `shouldParseQuery` Match
      [ Node $ LabelledNode (Just "p") "Person" M.empty
      , ConnectorDirection NoDirection
      , Relationship $ LabelledRelationship (Just "h") "HAS" M.empty
      , ConnectorDirection NoDirection
      , Node $ LabelledNode (Just "c") "Car" M.empty]
      Return

runParserMatchOddTests = do
  it "parses match clause with erratic spacing"
    $ "  MATCH  (   : Person{ name: ' D. A. V. E ' , age : 32 , height : 1.6 , delta : -10 , base  : -3.14  } )   -  [  o : OWNS  ] -> (car :Car )   RETURN    "
    `shouldParseQuery` Match
      [ Node
          $ LabelledNode
            Nothing
            "Person"
            (M.fromList
               [ ("age", IntegerValue 32)
               , ("base", DoubleValue (-3.14))
               , ("delta", IntegerValue (-10))
               , ("height", DoubleValue 1.6)
               , ("name", TextValue " D. A. V. E ")])
      , ConnectorDirection NoDirection
      , Relationship $ LabelledRelationship (Just "o") "OWNS" M.empty
      , ConnectorDirection RightDirection
      , Node $ LabelledNode (Just "car") "Car" M.empty]
      Return
  --
  -- it "fails on invalid clause producing correct error message"
  --   $ parse parseQuery "" "MARCH"
  --   `shouldFailWith` err
  --     0
  --     (utoks "MARCH" <> elabel "match clause or return clause")
  it "parses match clause followed by another match clause"
    $ "MATCH (n:Node) MATCH (n:Node) RETURN"
    `shouldParseQuery` Match
      [Node $ LabelledNode (Just "n") "Node" M.empty]
      (Match [Node $ LabelledNode (Just "n") "Node" M.empty] Return)

shouldParseQuery :: Text -> QueryExpr -> Expectation
shouldParseQuery query result = parse parseQuery "" query `shouldParse` result