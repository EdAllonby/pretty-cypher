-- This module houses the Parser type and the cypher language spacing rules that we use when parsing queries.
module Cypher.Parser.Core
  ( Parser,
    sc,
    lexeme,
    symbol,
    symbol',
    keyword',
    parens,
    brackets,
    curlyBrackets,
    parseText,
    integer,
    boolean,
    double,
    signedInteger,
    signedDouble,
    commaSep,
    parseLiteralText,
    parseLiteralTexts,
    parseWrappedInFunction,
    parsePropertyValue,
    parseClause,
    parseProperty,
    parseWildcard,
  )
where

import Control.Monad (void)
import Cypher.Types
  ( Function (Function),
    LiteralText (..),
    Object (..),
    Property (Property),
    PropertyValue (..),
  )
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec
  ( Parsec,
    between,
    choice,
    empty,
    lookAhead,
    manyTill,
    notFollowedBy,
    optional,
    sepBy,
    some,
    try,
    (<|>),
  )
import Text.Megaparsec.Char
  ( alphaNumChar,
    char,
    latin1Char,
    space1,
    string',
  )
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

symbol' :: Text -> Parser Text
symbol' = L.symbol' sc

integer :: Parser Integer
integer = lexeme L.decimal

boolean :: Parser Bool
boolean = choice [True <$ symbol' "true", False <$ symbol' "false"]

double :: Parser Double
double = lexeme L.float

signedInteger :: Parser Integer
signedInteger = L.signed sc integer

signedDouble :: Parser Double
signedDouble = L.signed sc double

keyword' :: Text -> Parser Text
keyword' keyword = lexeme (string' keyword <* notFollowedBy alphaNumChar)

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

curlyBrackets :: Parser a -> Parser a
curlyBrackets = between (symbol "{") (symbol "}")

parseText :: Parser Text
parseText = T.pack <$> lexeme (some alphaNumChar)

parseSnakeCaseText :: Parser Text
parseSnakeCaseText = T.pack <$> lexeme (some (alphaNumChar <|> char '_'))

textBetweenCharacter :: Char -> Parser Text
textBetweenCharacter boundingCharacter =
  T.pack
    <$> lexeme
      (between pCharacter pCharacter (manyTill latin1Char (lookAhead pCharacter)))
  where
    pCharacter = char boundingCharacter

commaSep :: Parser a -> Parser [a]
commaSep p = p `sepBy` symbol ","

parseLiteralText :: Parser LiteralText
parseLiteralText =
  choice
    [ QuotedText <$> (textBetweenCharacter '\'' <|> textBetweenCharacter '"'),
      BacktickedText <$> textBetweenCharacter '`',
      UnboundText <$> parseSnakeCaseText, -- TODO: It doesn't really matter if this is snake case, we need a more general text parser.
      UnboundText <$> parseText
    ]

parseLiteralTexts :: Parser [LiteralText]
parseLiteralTexts = commaSep parseLiteralText

parseWrappedInFunction :: Parser a -> Parser (Function a)
parseWrappedInFunction
  wrappedParser = Function <$> parseText <*> parens wrappedParser <*> parseAlias

parseObject :: Parser Object
parseObject =
  choice
    [ try $ NestedObject <$> (parseLiteralText <* symbol' ".") <*> parseObject,
      NestedObject <$> parseLiteralText <*> return ObjectEnd
    ]

hasObjectNesting :: Parser ()
hasObjectNesting = void $ lookAhead (parseLiteralText <* symbol' ".")

parsePropertyValue :: Parser PropertyValue
parsePropertyValue =
  choice
    [ DoubleValue <$> try signedDouble, -- TODO: Not too sure why this one needs a try, shouldn't it be atomic? Investigate
      IntegerValue <$> signedInteger,
      BooleanValue <$> boolean,
      ObjectValue <$> try (hasObjectNesting *> parseObject),
      ParamValue <$> parseParameter,
      TextValue <$> parseLiteralText,
      WildcardValue <$ symbol' "*"
    ]

parseClause :: Text -> Parser a -> Parser a
parseClause = (*>) . keyword'

parseProperty :: Parser Property
parseProperty =
  Property
    <$> parsePropertyValue
    <*> parseAlias

parseAlias :: Parser (Maybe LiteralText)
parseAlias = optional (symbol' "AS" *> parseLiteralText)

parseWildcard :: Parser Text
parseWildcard = symbol' "*"

parseParameter :: Parser Text
parseParameter = symbol' "$" *> parseText