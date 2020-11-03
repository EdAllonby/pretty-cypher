module Cypher.Parser.Delete where

import           Cypher.Types (LiteralText, Clause(DetachDelete, Delete))
import           Cypher.Parser.Core (parseLiteralText, Parser, keyword'
                                   , commaSep)
import           Control.Monad (void)

parseDelete :: Parser Clause
parseDelete = do
  void (keyword' "DELETE")
  Delete <$> parseLiteralTexts

parseDetachedDelete :: Parser Clause
parseDetachedDelete = do
  void (keyword' "DETACH DELETE")
  DetachDelete <$> parseLiteralTexts

parseLiteralTexts :: Parser [LiteralText]
parseLiteralTexts = commaSep parseLiteralText