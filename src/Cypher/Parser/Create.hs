module Cypher.Parser.Create where

import           Cypher.Types (Clause(Create))
import           Cypher.Parser.Core (Parser, keyword', commaSep)
import           Control.Monad (void)
import           Cypher.Parser.Pattern (parsePattern)

parseCreate :: Parser Clause
parseCreate = do
  void (keyword' "CREATE")
  Create <$> commaSep parsePattern
