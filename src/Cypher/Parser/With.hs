module Cypher.Parser.With (parseWith) where

import Cypher.Parser.Core
  ( Parser,
    parseClause,
    parseLiteralTexts,
  )
import Cypher.Types
  ( Clause (With),
  )

parseWith :: Parser Clause
parseWith = parseClause "WITH" (With <$> parseLiteralTexts)