module Cypher.Parser.Delete where

import           Cypher.Types (Clause(Delete))
import           Cypher.Parser.Core (parseLiteralText, Parser, keyword'
                                   , commaSep)
import           Control.Monad (void)

parseDelete :: Parser Clause
parseDelete = do
  void (keyword' "DELETE")
  Delete False <$> commaSep parseLiteralText

parseDetachedDelete :: Parser Clause
parseDetachedDelete = do
  void (keyword' "DETACH DELETE")
  Delete True <$> commaSep parseLiteralText

