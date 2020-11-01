-- This module houses the Parser type and the cypher language spacing rules that we use when parsing queries.
module Parser.Core
    ( Parser
    , sc
    , lexeme
    , symbol
    , symbol'
    , keyword'
    , parens
    , brackets
    , curlyBrackets
    , parseText
    , integer
    , boolean
    , double
    , signedInteger
    , signedDouble
    , commaSep
    , parseLiteralText) where

import           Data.Text (Text)
import           Data.Void (Void)
import           Text.Megaparsec (choice, (<|>), sepBy, lookAhead, manyTill
                                , some, between, notFollowedBy, empty, Parsec)
import           Text.Megaparsec.Char (latin1Char, char, alphaNumChar, space1
                                     , string')
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Text as T
import           Types (LiteralText(..))

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

betweenCharacter :: Char -> Parser Text
betweenCharacter boundingCharacter = T.pack
  <$> lexeme
    (between pCharacter pCharacter (manyTill latin1Char (lookAhead pCharacter)))
  where
    pCharacter = char boundingCharacter

commaSep :: Parser a -> Parser [a]
commaSep p = p `sepBy` symbol ","

parseLiteralText :: Parser LiteralText
parseLiteralText = choice
  [ QuotedText <$> (betweenCharacter '\'' <|> betweenCharacter '"')
  , BacktickedText <$> betweenCharacter '`'
  , UnboundText <$> parseSnakeCaseText -- TODO: It doesn't really matter if this is snake case, we need a more general text parser.
  , UnboundText <$> parseText]
