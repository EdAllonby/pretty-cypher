module Cypher.Parser.MatchSpec (runParserMatchTests) where

import           Cypher.Types
import           Cypher.Parser.Match
import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec
import           Data.Text as T
import qualified Data.Map as M

runParserMatchTests :: SpecWith ()
runParserMatchTests = describe "Cypher.Parser.Match"
  $ do
    it "parses match clause"
      $ "OPTIONAL MATCH (a)-[r:ACTS_IN]->()"
      `shouldParseOptionalMatchQuery` OptionalMatch
        [ MatchPattern
            Nothing
            [ Node (AnyPatternComponentType (UnboundText "a")) M.empty
            , ConnectorDirection NoDirection
            , Relationship
                (LabelledPatternComponentType
                   (Just (UnboundText "r"))
                   [UnboundText "ACTS_IN"])
                Nothing
                M.empty
            , ConnectorDirection RightDirection
            , Node EmptyPatternComponentType M.empty]]
    it "parses match with optional match clause"
      $ "OPTIONAL MATCH (a)-[r:ACTS_IN]->()"
      `shouldParseOptionalMatchQuery` OptionalMatch
        [ MatchPattern
            Nothing
            [ Node (AnyPatternComponentType (UnboundText "a")) M.empty
            , ConnectorDirection NoDirection
            , Relationship
                (LabelledPatternComponentType
                   (Just (UnboundText "r"))
                   [UnboundText "ACTS_IN"])
                Nothing
                M.empty
            , ConnectorDirection RightDirection
            , Node EmptyPatternComponentType M.empty]]
    it "parses single match clause with multiple patterns"
      $ "MATCH (p:Person), (m:Movie)"
      `shouldParseMatchQuery` Match
        [ MatchPattern Nothing [Node personLabelledNode M.empty]
        , MatchPattern
            Nothing
            [ Node
                (LabelledPatternComponentType
                   (Just (UnboundText "m"))
                   [UnboundText "Movie"])
                M.empty]]
    it "parses single match clause with multiple patterns"
      $ "OPTIONAL MATCH (p:Person), (m:Movie)"
      `shouldParseOptionalMatchQuery` OptionalMatch
        [ MatchPattern Nothing [Node personLabelledNode M.empty]
        , MatchPattern
            Nothing
            [ Node
                (LabelledPatternComponentType
                   (Just (UnboundText "m"))
                   [UnboundText "Movie"])
                M.empty]]
    it "parses pattern clause wrapped in a function"
      $ "MATCH shortestPath((p:Person)-[*]-(j:Job))"
      `shouldParseMatchQuery` Match
        [ MatchFunctionWrappedPattern
            Nothing
            (Function
               "shortestPath"
               [ Node personLabelledNode M.empty
               , ConnectorDirection NoDirection
               , Relationship EmptyPatternComponentType (Just AnyHops) M.empty
               , ConnectorDirection NoDirection
               , Node
                   (LabelledPatternComponentType
                      (Just (UnboundText "j"))
                      [UnboundText "Job"])
                   M.empty])]
    it "parses pattern with pattern variable wrapped in a function"
      $ "MATCH x=shortestPath((p:Person)-[*]-(j:Job))"
      `shouldParseMatchQuery` Match
        [ MatchFunctionWrappedPattern
            (Just "x")
            (Function
               "shortestPath"
               [ Node personLabelledNode M.empty
               , ConnectorDirection NoDirection
               , Relationship EmptyPatternComponentType (Just AnyHops) M.empty
               , ConnectorDirection NoDirection
               , Node
                   (LabelledPatternComponentType
                      (Just (UnboundText "j"))
                      [UnboundText "Job"])
                   M.empty])]

shouldParseMatchQuery :: Text -> Clause -> Expectation
shouldParseMatchQuery query expectedResult =
  parse parseMatch "" query `shouldParse` expectedResult

shouldParseOptionalMatchQuery :: Text -> Clause -> Expectation
shouldParseOptionalMatchQuery query expectedResult =
  parse parseOptionalMatch "" query `shouldParse` expectedResult

personLabelledNode :: PatternComponentType
personLabelledNode =
  LabelledPatternComponentType (Just (UnboundText "p")) [UnboundText "Person"]
