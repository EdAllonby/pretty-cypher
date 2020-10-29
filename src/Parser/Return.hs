module Parser.Return (parseReturn) where

import           Types (Clause(Return))
import           Parser.ParserCore (Parser, keyword')

parseReturn :: Parser Clause
parseReturn = do
  keyword' "RETURN"
  return Return

