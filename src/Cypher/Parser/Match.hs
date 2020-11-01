module Cypher.Parser.Match (parseMatch, parseOptionalMatch) where

import           Cypher.Types (Clause(OptionalMatch, Match))
import           Cypher.Parser.Core (Parser, keyword', commaSep)
import           Control.Monad (void)
import           Cypher.Parser.Pattern (parsePattern)

parseMatch :: Parser Clause
parseMatch = do
  void (keyword' "MATCH")
  Match <$> commaSep parsePattern

parseOptionalMatch :: Parser Clause
parseOptionalMatch = do
  void (keyword' "OPTIONAL MATCH")
  OptionalMatch <$> commaSep parsePattern
