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
        it "parses match clause with node"
          $ "MATCH (per:Person) RETURN"
          `shouldParseQuery` Match
            [Node $ LabelledNode (Just "per") "Person" M.empty]
            Return
        it "parses match clause with node specifying properties"
          $ "MATCH (per:Person { name: ' D. A. V. E '}) RETURN"
          `shouldParseQuery` Match
            [ Node
                $ LabelledNode
                  (Just "per")
                  "Person"
                  (M.fromList [("name", " D. A. V. E ")])]
            Return
        it "parses match clause with anonymous node"
          $ "MATCH (:Person) RETURN"
          `shouldParseQuery` Match
            [Node $ LabelledNode Nothing "Person" M.empty]
            Return
        it "parses match clause with empty node"
          $ "MATCH () RETURN" `shouldParseQuery` Match [Node EmptyNode] Return
        it "parses match clause with any node"
          $ "MATCH (n) RETURN"
          `shouldParseQuery` Match [Node $ AnyNode "n"] Return
        it "parses match clause with relationship"
          $ "MATCH [fo:FOLLOWS] RETURN"
          `shouldParseQuery` Match
            [Relationship $ LabelledRelationship (Just "fo") "FOLLOWS"]
            Return
        it "parses match clause with anonymous relationship"
          $ "MATCH [:FOLLOWS] RETURN"
          `shouldParseQuery` Match
            [Relationship $ LabelledRelationship Nothing "FOLLOWS"]
            Return
        it "parses match clause with any relationship"
          $ "MATCH [a] RETURN"
          `shouldParseQuery` Match [Relationship $ AnyRelationship "a"] Return
        it "parses match clause with right directionality"
          $ "MATCH (p:Person)-[h:HAS]->(c:Car) RETURN"
          `shouldParseQuery` Match
            [ Node $ LabelledNode (Just "p") "Person" M.empty
            , ConnectorDirection NoDirection
            , Relationship $ LabelledRelationship (Just "h") "HAS"
            , ConnectorDirection RightDirection
            , Node $ LabelledNode (Just "c") "Car" M.empty]
            Return
        it "parses match clause with left directionality"
          $ "MATCH (p:Person)<-[h:HAS]-(c:Car) RETURN"
          `shouldParseQuery` Match
            [ Node $ LabelledNode (Just "p") "Person" M.empty
            , ConnectorDirection LeftDirection
            , Relationship $ LabelledRelationship (Just "h") "HAS"
            , ConnectorDirection NoDirection
            , Node $ LabelledNode (Just "c") "Car" M.empty]
            Return
        it "parses match clause with no directionality"
          $ "MATCH (p:Person)-[h:HAS]-(c:Car) RETURN"
          `shouldParseQuery` Match
            [ Node $ LabelledNode (Just "p") "Person" M.empty
            , ConnectorDirection NoDirection
            , Relationship $ LabelledRelationship (Just "h") "HAS"
            , ConnectorDirection NoDirection
            , Node $ LabelledNode (Just "c") "Car" M.empty]
            Return
        it "parses match clause with extra spaces"
          $ "  MATCH  (   : Person   )   -  [  o : OWNS  ] -> (car :Car )   RETURN    "
          `shouldParseQuery` Match
            [ Node $ LabelledNode Nothing "Person" M.empty
            , ConnectorDirection NoDirection
            , Relationship $ LabelledRelationship (Just "o") "OWNS"
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