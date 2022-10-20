module Cypher.Parser.Pattern (parsePattern, parseProperty) where

import Control.Monad (void)
import Cypher.Parser.Core
  ( Parser,
    brackets,
    commaSep,
    curlyBrackets,
    integer,
    keyword',
    parens,
    parseLiteralText,
    parsePropertyValue,
    symbol,
  )
import Cypher.Types
  ( AnyPatternComponentTypeValue (AnyPatternComponentTypeValue),
    ConnectorDirection (..),
    LabelledPatternComponentTypeValue (LabelledPatternComponentTypeValue),
    LiteralText,
    Pattern,
    PatternComponent (ConnectorDirection, Node, Relationship),
    PatternComponentType
      ( AnyPatternComponentType,
        EmptyPatternComponentType,
        LabelledPatternComponentType
      ),
    PropertyValue,
    RelationshipHops (..),
  )
import Data.Map qualified as M
import Text.Megaparsec
  ( MonadParsec (lookAhead, try),
    choice,
    eof,
    manyTill,
    optional,
    sepBy1,
    (<?>),
    (<|>),
  )

parsePattern :: Parser Pattern
parsePattern =
  manyTill
    ( choice
        [ parens (Node <$> parseNodeType <*> parseProperties) <?> "valid node",
          brackets
            ( Relationship
                <$> parseRelationshipType
                <*> optional parseRelationshipHops
                <*> parseProperties
            )
            <?> "valid relationship",
          ConnectorDirection <$> parseConnectorDirection <?> "connector"
        ]
    )
    -- Can we unify these lookahead tokens with those specified in the above parseQuery table?
    ( lookAhead . choice $
        [ void $ keyword' "RETURN",
          void $ keyword' "MATCH",
          void $ keyword' "OPTIONAL MATCH",
          void $ keyword' "WITH",
          void $ keyword' "DELETE",
          void $ keyword' "DETACH DELETE",
          void $ symbol ",",
          void $ symbol ")",
          eof -- TODO: Really don't want to have this EOF, but we have it here otherwise we need to add RETURN statements to tests. Is there another option?
        ]
    )

parseNodeType :: Parser PatternComponentType
parseNodeType = parsePatternComponentType (symbol ":" *> parseLiteralText `sepBy1` symbol ":")

parseRelationshipType :: Parser PatternComponentType
parseRelationshipType = parsePatternComponentType (symbol ":" *> parseLiteralText `sepBy1` (symbol "|:" <|> symbol "|"))

parsePatternComponentType :: Parser [LiteralText] -> Parser PatternComponentType
parsePatternComponentType labelsParser =
  choice
    [ try $
        LabelledPatternComponentType
          <$> ( LabelledPatternComponentTypeValue
                  <$> optional parseLiteralText
                  <*> labelsParser
              ),
      parseAnyComponentType,
      EmptyPatternComponentType <$ symbol ""
    ]

parseAnyComponentType :: Parser PatternComponentType
parseAnyComponentType = AnyPatternComponentType . AnyPatternComponentTypeValue <$> parseLiteralText

parseRelationshipHops :: Parser RelationshipHops
parseRelationshipHops =
  symbol "*"
    *> choice
      [ try (VariableHops <$> integer <*> (symbol ".." *> integer)),
        try (MinHops <$> (integer <* symbol "..")),
        MaxHops <$> (symbol ".." *> integer),
        FixedHops <$> integer,
        return AnyHops
      ]

parseConnectorDirection :: Parser ConnectorDirection
parseConnectorDirection =
  choice
    [ AnonymousRightDirection <$ symbol "-->",
      RightDirection <$ symbol "->",
      AnonymousNoDirection <$ symbol "--",
      NoDirection <$ symbol "-",
      AnonymousLeftDirection <$ symbol "<--",
      LeftDirection <$ symbol "<-"
    ]

parseProperties :: Parser (M.Map LiteralText PropertyValue)
parseProperties = do
  props <- optional $ curlyBrackets $ commaSep parseProperty
  return $ M.unions (M.fromList <$> props)

parseProperty :: Parser (LiteralText, PropertyValue)
parseProperty = (,) <$> parseLiteralText <*> (symbol ":" *> parsePropertyValue)
