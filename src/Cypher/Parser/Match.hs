module Cypher.Parser.Match (parseMatch, parseOptionalMatch) where

import Cypher.Parser.Core
  ( Parser,
    commaSep,
    parseClause,
    parseText,
    parseWrappedInFunction,
    symbol,
  )
import Cypher.Parser.Pattern (parsePattern)
import Cypher.Types
  ( Clause (Match, OptionalMatch),
    MatchFunctionWrappedPatternValue (..),
    MatchPatternValue (..),
    MatchValue (MatchFunctionWrappedPattern, MatchPattern),
  )
import Text.Megaparsec (choice, optional, try)

parseMatch :: Parser Clause
parseMatch = parseClause "MATCH" (Match <$> commaSep parseMatchValue)

parseOptionalMatch :: Parser Clause
parseOptionalMatch =
  parseClause "OPTIONAL MATCH" (OptionalMatch <$> commaSep parseMatchValue)

parseMatchValue :: Parser MatchValue
parseMatchValue = do
  patternVariable <- optional $ try $ parseText <* symbol "="
  choice
    [ try $ MatchFunctionWrappedPattern <$> (MatchFunctionWrappedPatternValue patternVariable <$> parseWrappedInFunction parsePattern),
      MatchPattern <$> (MatchPatternValue patternVariable <$> parsePattern)
    ]
