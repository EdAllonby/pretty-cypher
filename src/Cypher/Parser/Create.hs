module Cypher.Parser.Create where

import           Cypher.Types (Clause(Create))
import           Cypher.Parser.Core (parseClause, Parser, commaSep)
import           Cypher.Parser.Pattern (parsePattern)

parseCreate :: Parser Clause
parseCreate = parseClause "CREATE" (Create <$> commaSep parsePattern)