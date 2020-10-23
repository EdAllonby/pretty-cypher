-- This module houses the Parser type and the cypher language spacing rules that we use when parsing queries.
module ParserCore
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
    , betweenQuotes) where

import           Data.Text (Text)
import           Data.Void (Void)
import           Text.Megaparsec (lookAhead, manyTill, some, between
                                , notFollowedBy, empty, Parsec)
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

betweenQuotes :: Parser Text
betweenQuotes = T.pack
  <$> lexeme (between pQuote pQuote (manyTill latin1Char (lookAhead pQuote)))
  where
    pQuote = char '\''