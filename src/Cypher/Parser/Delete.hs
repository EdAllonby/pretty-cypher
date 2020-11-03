module Cypher.Parser.Delete where

import           Cypher.Types (LiteralText, Clause(DetachDelete, Delete))
import           Cypher.Parser.Core (parseClause, parseLiteralText, Parser
                                   , commaSep)

parseDelete :: Parser Clause
parseDelete = parseClause "DELETE" (Delete <$> parseLiteralTexts)

parseDetachedDelete :: Parser Clause
parseDetachedDelete =
  parseClause "DETACH DELETE" (DetachDelete <$> parseLiteralTexts)

parseLiteralTexts :: Parser [LiteralText]
parseLiteralTexts = commaSep parseLiteralText
