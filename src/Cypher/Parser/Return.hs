module Cypher.Parser.Return (parseReturn) where

import Cypher.Parser.Core
  ( Parser,
    commaSep,
    parseClause,
    parseLiteralText,
    parsePropertyValue,
    parseWrappedInFunction,
    symbol',
  )
import Cypher.Parser.Pattern (parsePattern)
import Cypher.Types
  ( Clause (Return),
    ReturnExpression (..),
    ReturnProperty (Property),
    ReturnValue (..),
  )
import Data.Maybe (isJust)
import Text.Megaparsec (MonadParsec (try), choice, optional)

parseReturn :: Parser Clause
parseReturn =
  parseClause "RETURN" (Return <$> parseHasDistinct <*> parseReturnValue)

parseHasDistinct :: Parser Bool
parseHasDistinct = isJust <$> (optional . symbol' $ "DISTINCT")

parseReturnValue :: Parser ReturnValue
parseReturnValue =
  choice
    [ ReturnAllElements <$ symbol' "*",
      ReturnExpressions <$> commaSep parseReturnExpression
    ]

parseReturnExpression :: Parser ReturnExpression
parseReturnExpression =
  choice
    [ try $ ReturnFunctionWrappedPattern <$> parseWrappedInFunction parsePattern,
      try $
        ReturnFunctionWrappedPropertyWithArity
          <$> parseWrappedInFunction (commaSep parsePropertyValue),
      ReturnPattern <$> parsePattern,
      ReturnProperty <$> parseReturnProperty
    ]

parseReturnProperty :: Parser ReturnProperty
parseReturnProperty =
  Property <$> parsePropertyValue
    <*> optional (symbol' "AS" *> parseLiteralText)
