module Parser.Return (parseReturn) where

import           Types (Clause(Return), ReturnValue(..), Object(..)
                      , ReturnProperty(Property))
import           Parser.ParserCore (commaSep, Parser, symbol', keyword'
                                  , parseLiteralText)
import           Text.Megaparsec
import           Control.Monad (void)

parseReturn :: Parser Clause
parseReturn = do
  void (keyword' "RETURN")
  Return <$> parseReturnValue

parseReturnValue :: Parser ReturnValue
parseReturnValue = choice
  [ ReturnAllElements <$ symbol' "*"
  , ReturnProperties <$> commaSep parseReturnProperty]

parseReturnProperty :: Parser ReturnProperty
parseReturnProperty = Property <$> parseObject
  <*> optional (symbol' "AS" *> parseLiteralText)

parseObject :: Parser Object
parseObject = choice
  [ try $ NestedObject <$> (parseLiteralText <* symbol' ".") <*> parseObject
  , NestedObject <$> parseLiteralText <*> return ObjectEnd]