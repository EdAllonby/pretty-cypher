module Cypher.Parser.Create (parseCreate) where

import Cypher.Parser.Core (Parser, commaSep, parseClause)
import Cypher.Parser.Pattern (parsePattern)
import Cypher.Types (Clause (Create))

parseCreate :: Parser Clause
parseCreate = parseClause "CREATE" (Create <$> commaSep parsePattern)
