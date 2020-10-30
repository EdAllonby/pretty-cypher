module Parser.Return (parseReturn) where

import           Types (Clause(Return), ReturnValue(..))
import           Parser.ParserCore (symbol, parseLiteralText, Parser, keyword')
import           Text.Megaparsec

parseReturn :: Parser Clause
parseReturn = do
  keyword' "RETURN"
  Return <$> parseReturnValue

parseReturnValue :: Parser ReturnValue
parseReturnValue =
  choice [AllElements <$ symbol "*", Property <$> parseLiteralText]