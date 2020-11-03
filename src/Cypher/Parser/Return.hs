module Cypher.Parser.Return (parseReturn) where

import           Cypher.Types (Clause(Return), ReturnValue(..)
                             , ReturnExpression(..), ReturnProperty(Property))
import           Cypher.Parser.Pattern (parsePattern)
import           Cypher.Parser.Core (parseClause, parsePropertyValue
                                   , parseWrappedInFunction, commaSep, Parser
                                   , symbol', parseLiteralText)
import           Text.Megaparsec (optional, choice, MonadParsec(try))
import           Data.Maybe (isJust)

parseReturn :: Parser Clause
parseReturn =
  parseClause "RETURN" (Return <$> parseHasDistinct <*> parseReturnValue)

parseHasDistinct :: Parser Bool
parseHasDistinct = isJust <$> (optional . symbol' $ "DISTINCT")

parseReturnValue :: Parser ReturnValue
parseReturnValue = choice
  [ ReturnAllElements <$ symbol' "*"
  , ReturnExpressions <$> commaSep parseReturnExpression]

parseReturnExpression :: Parser ReturnExpression
parseReturnExpression = choice
  [ try $ ReturnFunctionWrappedPattern <$> parseWrappedInFunction parsePattern
  , try
      $ ReturnFunctionWrappedPropertyWithArity
      <$> parseWrappedInFunction (commaSep parsePropertyValue)
  , ReturnPattern <$> parsePattern
  , ReturnProperty <$> parseReturnProperty]

parseReturnProperty :: Parser ReturnProperty
parseReturnProperty = Property <$> parsePropertyValue
  <*> optional (symbol' "AS" *> parseLiteralText)
