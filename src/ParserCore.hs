-- This module houses the Parser type and the cypher language spacing rules that we use when parsing queries.
module ParserCore (Parser, sc, lexeme, symbol, symbol') where

import           Data.Text (Text)
import           Data.Void (Void)
import           Text.Megaparsec (empty, Parsec)
import           Text.Megaparsec.Char (space1)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

symbol' :: Text -> Parser Text
symbol' = L.symbol' sc
