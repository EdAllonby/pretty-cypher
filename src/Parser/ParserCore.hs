-- This module houses the Parser type and the cypher language spacing rules that we use when parsing queries.
module Parser.ParserCore
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
    , parseSnakeCaseText
    , betweenQuotes
    , integer
    , double
    , signedInteger
    , signedDouble
    , commaSep) where

import           Data.Text (Text)
import           Data.Void (Void)
import           Text.Megaparsec ((<|>), sepBy, lookAhead, manyTill, some
                                , between, notFollowedBy, empty, Parsec)
import           Text.Megaparsec.Char (latin1Char, char, letterChar
                                     , alphaNumChar, space1, string')
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Text as T

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
parseText = T.pack <$> lexeme (some letterChar)

parseSnakeCaseText :: Parser Text
parseSnakeCaseText = T.pack <$> lexeme (some (letterChar <|> char '_'))

betweenQuotes :: Parser Text
betweenQuotes = T.pack
  <$> lexeme (between pQuote pQuote (manyTill latin1Char (lookAhead pQuote)))
  where
    pQuote = char '\''

commaSep :: Parser a -> Parser [a]
commaSep p = p `sepBy` symbol ","