module Parser.Return (parseReturn) where

import           Types (Clause(Return), ReturnValue(..), Object(..))
import           Parser.ParserCore (Parser, symbol, keyword', parseLiteralText)
import           Text.Megaparsec (choice, MonadParsec(try))

parseReturn :: Parser Clause
parseReturn = do
  keyword' "RETURN"
  Return <$> parseReturnValue

parseReturnValue :: Parser ReturnValue
parseReturnValue = choice [AllElements <$ symbol "*", Property <$> parseObject]

parseObject :: Parser Object
parseObject = choice
  [ try $ NestedObject <$> (parseLiteralText <* symbol ".") <*> parseObject
  , NestedObject <$> parseLiteralText <*> return ObjectEnd]