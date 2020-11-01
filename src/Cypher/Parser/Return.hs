module Cypher.Parser.Return (parseReturn) where

import           Cypher.Types (Clause(Return), ReturnValue(..)
                             , ReturnExpression(..), ReturnProperty(Property)
                             , Object(..))
import           Cypher.Parser.Pattern (parsePattern)
import           Cypher.Parser.Core (commaSep, Parser, symbol', keyword'
                                   , parseLiteralText)
import           Text.Megaparsec (optional, choice, MonadParsec(try))
import           Control.Monad (void)

parseReturn :: Parser Clause
parseReturn = do
  void (keyword' "RETURN")
  Return <$> parseReturnValue

parseReturnValue :: Parser ReturnValue
parseReturnValue = choice
  [ ReturnAllElements <$ symbol' "*"
  , ReturnExpressions <$> commaSep parseReturnExpression]

parseReturnExpression :: Parser ReturnExpression
parseReturnExpression = choice
  [ReturnPattern <$> parsePattern, ReturnProperty <$> parseReturnProperty]

parseReturnProperty :: Parser ReturnProperty
parseReturnProperty = Property <$> parseObject
  <*> optional (symbol' "AS" *> parseLiteralText)

parseObject :: Parser Object
parseObject = choice
  [ try $ NestedObject <$> (parseLiteralText <* symbol' ".") <*> parseObject
  , NestedObject <$> parseLiteralText <*> return ObjectEnd]