module Cypher.Parser.Return (parseReturn) where

import Cypher.Parser.Core
  ( Parser,
    commaSep,
    parseClause,
    parseProperty,
    parsePropertyValue,
    parseWildcard,
    parseWrappedInFunction,
    symbol',
  )
import Cypher.Parser.Pattern (parsePattern)
import Cypher.Types
  ( Clause (Return),
    ReturnClause (..),
    ReturnExpression (..),
    ReturnValue (..),
  )
import Data.Maybe (isJust)
import Text.Megaparsec (MonadParsec (try), choice, optional)

parseReturn :: Parser Clause
parseReturn = parseClause "RETURN" (Return <$> parseReturnClause)

parseReturnClause :: Parser ReturnClause
parseReturnClause = ReturnClause <$> parseHasDistinct <*> parseReturnValue

parseHasDistinct :: Parser Bool
parseHasDistinct = isJust <$> (optional . symbol' $ "DISTINCT")

parseReturnValue :: Parser ReturnValue
parseReturnValue =
  choice
    [ ReturnAllElements <$ parseWildcard,
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
      ReturnProperty <$> parseProperty
    ]
