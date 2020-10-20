module Parser (parseQuery, Node(..), QueryExpr(..)) where

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data Clause = ClauseMatch
            | ClauseReturn

data Node = Node { nodeAlias :: Char, nodeLabel :: Text }
  deriving (Show, Eq)

data QueryExpr = Match Node QueryExpr
               | Return
               | End
  deriving (Eq, Show)

type Parser = Parsec Void Text

parseQuery :: Parser QueryExpr
parseQuery = do
  sc
  clause <- parseClause
  match <- parseMatch
  eof
  return match

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

symbol' :: Text -> Parser Text
symbol' = L.symbol' sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pKeyword :: Text -> Parser Text
pKeyword keyword = lexeme (string keyword)

pKeyword' :: Text -> Parser Text
pKeyword' keyword = lexeme (string keyword <* notFollowedBy alphaNumChar)

parseClause :: Parser Clause
parseClause = choice
  [ClauseMatch <$ symbol' "MATCH", ClauseReturn <$ symbol' "RETURN"]
  <?> "valid clause"

parseMatch :: Parser QueryExpr
parseMatch = do
  sc
  node <- parens parseNode
  eof
  return (Match node End)

parseNode :: Parser Node
parseNode = Node <$> lowerChar
  <*> (char ':' *> (T.pack <$> lexeme (some letterChar)))