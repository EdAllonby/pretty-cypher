module Cypher.Parser.CreateSpec (runParserCreateTests) where

import           Cypher.Types (Clause(Create), LiteralText(UnboundText)
                             , PatternComponent(Node)
                             , PatternComponentType(LabelledPatternComponentType))
import           Test.Hspec (describe, it, SpecWith, Expectation)
import           Test.Hspec.Megaparsec (shouldParse)
import           Text.Megaparsec (parse)
import           Data.Text as T (Text)
import qualified Data.Map as M
import           Cypher.Parser.Create (parseCreate)

runParserCreateTests :: SpecWith ()
runParserCreateTests = describe "Cypher.Parser.Create"
  $ do
    it "parses single create clause with single pattern"
      $ "CREATE (p:Person)"
      `shouldParseCreateQuery` Create [[Node personLabelledNode M.empty]]
    it "parses single create clause with multiple patterns"
      $ "CREATE (p:Person), (m:Movie)"
      `shouldParseCreateQuery` Create
        [ [Node personLabelledNode M.empty]
        , [ Node
              (LabelledPatternComponentType
                 (Just (UnboundText "m"))
                 [UnboundText "Movie"])
              M.empty]]

shouldParseCreateQuery :: Text -> Clause -> Expectation
shouldParseCreateQuery query expectedResult =
  parse parseCreate "" query `shouldParse` expectedResult

personLabelledNode :: PatternComponentType
personLabelledNode =
  LabelledPatternComponentType (Just (UnboundText "p")) [UnboundText "Person"]
