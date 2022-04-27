module Cypher.Parser.Delete where

import Cypher.Parser.Core
  ( Parser,
    commaSep,
    parseClause,
    parseLiteralText,
  )
import Cypher.Types (Clause (Delete, DetachDelete), LiteralText)

parseDelete :: Parser Clause
parseDelete = parseClause "DELETE" (Delete <$> parseLiteralTexts)

parseDetachedDelete :: Parser Clause
parseDetachedDelete =
  parseClause "DETACH DELETE" (DetachDelete <$> parseLiteralTexts)

parseLiteralTexts :: Parser [LiteralText]
parseLiteralTexts = commaSep parseLiteralText
