module Cypher.Parser.Match (parseMatch, parseOptionalMatch) where

import           Cypher.Types (Clause(OptionalMatch, Match)
                             , MatchValue(MatchPattern, MatchFunctionWrappedPattern))
import           Cypher.Parser.Core (parseWrappedInFunction, symbol, parseText
                                   , Parser, keyword', commaSep)
import           Control.Monad (void)
import           Cypher.Parser.Pattern (parsePattern)
import           Text.Megaparsec (choice, try, optional)

parseMatch :: Parser Clause
parseMatch = do
  void (keyword' "MATCH")
  Match <$> commaSep parseMatchValue

parseOptionalMatch :: Parser Clause
parseOptionalMatch = do
  void (keyword' "OPTIONAL MATCH")
  OptionalMatch <$> commaSep parseMatchValue

parseMatchValue :: Parser MatchValue
parseMatchValue = do
  patternVariable <- optional $ try $ parseText <* symbol "="
  choice
    [ try
        $ MatchFunctionWrappedPattern patternVariable
        <$> parseWrappedInFunction parsePattern
    , MatchPattern patternVariable <$> parsePattern]