module Cypher.Parser.Match (parseMatch, parseOptionalMatch) where

import           Cypher.Types (Clause(OptionalMatch, Match)
                             , MatchValue(MatchPattern, MatchFunctionWrappedPattern))
import           Cypher.Parser.Core (parseClause, parseWrappedInFunction, symbol
                                   , parseText, Parser, commaSep)
import           Cypher.Parser.Pattern (parsePattern)
import           Text.Megaparsec (choice, try, optional)

parseMatch :: Parser Clause
parseMatch = parseClause "MATCH" (Match <$> commaSep parseMatchValue)

parseOptionalMatch :: Parser Clause
parseOptionalMatch =
  parseClause "OPTIONAL MATCH" (OptionalMatch <$> commaSep parseMatchValue)

parseMatchValue :: Parser MatchValue
parseMatchValue = do
  patternVariable <- optional $ try $ parseText <* symbol "="
  choice
    [ try
        $ MatchFunctionWrappedPattern patternVariable
        <$> parseWrappedInFunction parsePattern
    , MatchPattern patternVariable <$> parsePattern]