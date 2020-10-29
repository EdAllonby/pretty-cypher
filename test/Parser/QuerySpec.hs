{-# LANGUAGE QuasiQuotes #-}

module Parser.QuerySpec where

import           Types
import           Parser.Query
import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec
import           Data.Text as T
import qualified Data.Map as M
import           Text.RawString.QQ (r)

runParserQueryTests :: SpecWith ()
runParserQueryTests = describe "Parser.Query"
  $ do
    context "when parsing queries"
      $ do
        context "with complex query" runParserComplexQueryTests
        context "with error" runParserQueryErrorTests

runParserComplexQueryTests = do
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
                               [ Node (AnyNode "a") M.empty
                               , ConnectorDirection NoDirection
                               , Relationship
                                   (LabelledRelationship (Just "r") ["ACTS_IN"])
                                   Nothing
                                   M.empty
                               , ConnectorDirection RightDirection
                               , Node EmptyNode M.empty]]
                       , Return]
  it "parses query with erratic spacing"
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
                                   (LabelledRelationship (Just "o") ["OWNS"])
                                   Nothing
                                   M.empty
                               , ConnectorDirection RightDirection
                               , Node
                                   (LabelledNode (Just "car") ["Car"])
                                   M.empty]]
                       , Return]
  it "parses multi match clause"
    $ "MATCH (p:Person)-[:HAS]->(c:Car) MATCH (cat:Cat) RETURN"
    `shouldParseQuery` [ Match
                           [ Pattern
                               Nothing
                               [ Node
                                   (LabelledNode (Just "p") ["Person"])
                                   M.empty
                               , ConnectorDirection NoDirection
                               , Relationship
                                   (LabelledRelationship Nothing ["HAS"])
                                   Nothing
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

runParserQueryErrorTests = do
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