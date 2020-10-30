module Parser.Return (parseReturn) where

import           Types (Clause(Return))
import           Parser.ParserCore (parseLiteralText, Parser, keyword')

parseReturn :: Parser Clause
parseReturn = do
  keyword' "RETURN"
  Return <$> parseLiteralText