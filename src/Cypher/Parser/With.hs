module Cypher.Parser.With (parseWith) where

import Cypher.Parser.Core
  ( Parser,
    commaSep,
    parseClause,
    parseProperty,
    parseWildcard,
    parseWrappedInFunction,
  )
import Cypher.Types
  ( Clause (With),
    WithValue (WithFunctionWrappedProperty, WithProperty, WithWildcard),
  )
import Text.Megaparsec (MonadParsec (try), choice)

parseWith :: Parser Clause
parseWith = parseClause "WITH" (With <$> parseWithValues)

parseWithValues :: Parser [WithValue]
parseWithValues =
  commaSep
    ( choice
        [ WithWildcard <$ parseWildcard,
          try $ WithFunctionWrappedProperty <$> parseWrappedInFunction parseProperty,
          WithProperty <$> parseProperty
        ]
    )
