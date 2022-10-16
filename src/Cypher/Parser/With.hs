module Cypher.Parser.With (parseWith) where

import Cypher.Parser.Core
  ( Parser,
    commaSep,
    parseClause,
    parseProperty,
  )
import Cypher.Types
  ( Clause (With),
    WithValue (WithProperty),
  )

parseWith :: Parser Clause
parseWith = parseClause "WITH" (With <$> parseWithValues)

parseWithValues :: Parser [WithValue]
parseWithValues = commaSep (WithProperty <$> parseProperty)
