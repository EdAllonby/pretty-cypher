{-# LANGUAGE QuasiQuotes #-}

module Cypher.Parser.QuerySpec (runParserQueryTests) where

import Cypher.Parser.Query
import Cypher.Types
import Data.Map qualified as M
import Data.Text as T
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Text.RawString.QQ (r)

runParserQueryTests :: SpecWith ()
runParserQueryTests = describe "Cypher.Parser.Query" $
  do
    context "when parsing queries" $
      do
        context "with complex query" runParserComplexQueryTests
        context "with error" runParserQueryErrorTests

runParserComplexQueryTests :: Spec
runParserComplexQueryTests = do
  it "parses match with optional match clause" $
    [r|
MATCH (a:Movie { title: 'Wall Street' })
OPTIONAL MATCH (a)-[r:ACTS_IN]->()
RETURN *
    |]
      `shouldParseQuery` [ Match
                             [ MatchPattern
                                 Nothing
                                 [ Node
                                     ( LabelledPatternComponentType
                                         (Just (UnboundText "a"))
                                         [UnboundText "Movie"]
                                     )
                                     ( M.fromList
                                         [ ( UnboundText "title",
                                             TextValue
                                               (QuotedText "Wall Street")
                                           )
                                         ]
                                     )
                                 ]
                             ],
                           OptionalMatch
                             [ MatchPattern
                                 Nothing
                                 [ Node
                                     (AnyPatternComponentType (UnboundText "a"))
                                     M.empty,
                                   ConnectorDirection NoDirection,
                                   Relationship
                                     ( LabelledPatternComponentType
                                         (Just (UnboundText "r"))
                                         [UnboundText "ACTS_IN"]
                                     )
                                     Nothing
                                     M.empty,
                                   ConnectorDirection RightDirection,
                                   Node EmptyPatternComponentType M.empty
                                 ]
                             ],
                           Return False ReturnAllElements
                         ]
  it "parses query with erratic spacing" $
    "  MATCH  (   : Person{ name: ' D. A. V. E ' , age : 32 , height : 1.6 , delta : -10 , base  : -3.14  } )   -  [  o : OWNS  ] -> (car :Car )   RETURN  *   "
      `shouldParseQuery` [ Match
                             [ MatchPattern
                                 Nothing
                                 [ Node
                                     ( LabelledPatternComponentType
                                         Nothing
                                         [UnboundText "Person"]
                                     )
                                     ( M.fromList
                                         [ (UnboundText "age", IntegerValue 32),
                                           ( UnboundText "base",
                                             DoubleValue (-3.14)
                                           ),
                                           ( UnboundText "delta",
                                             IntegerValue (-10)
                                           ),
                                           (UnboundText "height", DoubleValue 1.6),
                                           ( UnboundText "name",
                                             TextValue
                                               (QuotedText " D. A. V. E ")
                                           )
                                         ]
                                     ),
                                   ConnectorDirection NoDirection,
                                   Relationship
                                     ( LabelledPatternComponentType
                                         (Just (UnboundText "o"))
                                         [UnboundText "OWNS"]
                                     )
                                     Nothing
                                     M.empty,
                                   ConnectorDirection RightDirection,
                                   Node
                                     ( LabelledPatternComponentType
                                         (Just (UnboundText "car"))
                                         [UnboundText "Car"]
                                     )
                                     M.empty
                                 ]
                             ],
                           Return False ReturnAllElements
                         ]
  it "parses multi match clause" $
    "MATCH (p:Person)-[:HAS]->(c:Car) MATCH (cat:Cat) RETURN c"
      `shouldParseQuery` [ Match
                             [ MatchPattern
                                 Nothing
                                 [ Node
                                     ( LabelledPatternComponentType
                                         (Just (UnboundText "p"))
                                         [UnboundText "Person"]
                                     )
                                     M.empty,
                                   ConnectorDirection NoDirection,
                                   Relationship
                                     ( LabelledPatternComponentType
                                         Nothing
                                         [UnboundText "HAS"]
                                     )
                                     Nothing
                                     M.empty,
                                   ConnectorDirection RightDirection,
                                   Node
                                     ( LabelledPatternComponentType
                                         (Just (UnboundText "c"))
                                         [UnboundText "Car"]
                                     )
                                     M.empty
                                 ]
                             ],
                           Match
                             [ MatchPattern
                                 Nothing
                                 [ Node
                                     ( LabelledPatternComponentType
                                         (Just (UnboundText "cat"))
                                         [UnboundText "Cat"]
                                     )
                                     M.empty
                                 ]
                             ],
                           Return
                             False
                             ( ReturnExpressions
                                 [ ReturnProperty
                                     ( Property
                                         (TextValue (UnboundText "c"))
                                         Nothing
                                     )
                                 ]
                             )
                         ]

runParserQueryErrorTests :: SpecWith ()
runParserQueryErrorTests = do
  it "fails on invalid clause producing correct error message" $
    "MARCH"
      `shouldFailQuery` ( utoks "MARCH"
                            <> elabel "match clause"
                            <> elabel "optional match clause"
                            <> elabel "create clause"
                            <> elabel "delete clause"
                            <> elabel "detach delete clause"
                            <> elabel "return clause"
                            <> eeof
                        )

shouldParseQuery :: Text -> QueryExpr -> Expectation
shouldParseQuery query expectedResult =
  parse parseQuery "" query `shouldParse` expectedResult

shouldFailQuery :: Text -> ET Text -> Expectation
shouldFailQuery query expectedError =
  parse parseQuery "" query `shouldFailWith` err 0 expectedError