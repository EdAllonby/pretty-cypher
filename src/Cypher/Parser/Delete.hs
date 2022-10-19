module Cypher.Parser.Delete (parseDelete, parseDetachedDelete) where

import Cypher.Parser.Core
  ( Parser,
    parseClause,
    parseLiteralTexts,
  )
import Cypher.Types (Clause (Delete, DetachDelete))

parseDelete :: Parser Clause
parseDelete = parseClause "DELETE" (Delete <$> parseLiteralTexts)

parseDetachedDelete :: Parser Clause
parseDetachedDelete =
  parseClause "DETACH DELETE" (DetachDelete <$> parseLiteralTexts)
