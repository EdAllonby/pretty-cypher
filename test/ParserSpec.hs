module ParserSpec where

import           Parser
import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec
import           Data.Text

runParserTests :: SpecWith ()
runParserTests = describe "Parser"
  $ do
    context "When parsing match only query"
      $ do
        it "parses match clause with node"
          $ "MATCH (p:Person) RETURN"
          `shouldParseQuery` Match [Node 'p' "Person"] Return
        it "parses match clause with relationship"
          $ "MATCH [f:FOLLOWS] RETURN"
          `shouldParseQuery` Match [Relationship 'f' "FOLLOWS"] Return
        it "parses match clause with right directionality"
          $ "MATCH (p:Person)-[h:HAS]->(c:Car) RETURN"
          `shouldParseQuery` Match
            [ Node 'p' "Person"
            , ConnectorDirection NoDirection
            , Relationship 'h' "HAS"
            , ConnectorDirection RightDirection
            , Node 'c' "Car"]
            Return
        it "parses match clause with left directionality"
          $ "MATCH (p:Person)<-[h:HAS]-(c:Car) RETURN"
          `shouldParseQuery` Match
            [ Node 'p' "Person"
            , ConnectorDirection LeftDirection
            , Relationship 'h' "HAS"
            , ConnectorDirection NoDirection
            , Node 'c' "Car"]
            Return
        it "parses match clause with no directionality"
          $ "MATCH (p:Person)-[h:HAS]-(c:Car) RETURN"
          `shouldParseQuery` Match
            [ Node 'p' "Person"
            , ConnectorDirection NoDirection
            , Relationship 'h' "HAS"
            , ConnectorDirection NoDirection
            , Node 'c' "Car"]
            Return
        it "parses match clause with extra spaces"
          $ "  MATCH  (  p : Person   )   ->  [  o : OWNS  ]    RETURN    "
          `shouldParseQuery` Match
            [ Node 'p' "Person"
            , ConnectorDirection RightDirection
            , Relationship 'o' "OWNS"]
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
            [Node { nodeAlias = 'n', nodeLabel = "Node" }]
            (Match [Node { nodeAlias = 'n', nodeLabel = "Node" }] Return)

shouldParseQuery :: Text -> QueryExpr -> Expectation
shouldParseQuery query result = parse parseQuery "" query `shouldParse` result