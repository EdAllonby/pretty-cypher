module Cypher.Parser.With (parseWith) where

import Cypher.Parser.Core
  ( Parser,
    commaSep,
    parseClause,
    parseProperty,
    symbol',
  )
import Cypher.Types
  ( Clause (With),
    WithValue (WithProperty, WithWildcard),
  )
import Text.Megaparsec (choice)

parseWith :: Parser Clause
parseWith = parseClause "WITH" (With <$> parseWithValues)

parseWithValues :: Parser [WithValue]
parseWithValues = commaSep (choice [WithWildcard <$ symbol' "*", WithProperty <$> parseProperty])
