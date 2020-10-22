module Parser where

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data ConnectorDirection = LeftDirection
                        | RightDirection
                        | NoDirection
  deriving (Show, Eq)

data MatchSection = Node { nodeVariable :: Maybe Text, nodeLabel :: Text }
                  | Relationship { relationshipVariable :: Maybe Text
                                 , relationshipLabel :: Text
                                 }
                  | ConnectorDirection ConnectorDirection
                  | Direction
  deriving (Show, Eq)

data QueryExpr = Match [MatchSection] QueryExpr
               | Return
  deriving (Eq, Show)

type Parser = Parsec Void Text

pTerm :: Parser QueryExpr
pTerm = choice
  [ (parseMatch <*> parseQuery) <?> "match clause"
  , parseReturn <?> "return clause"]

parseQuery :: Parser QueryExpr
parseQuery = do
  sc
  term <- pTerm
  eof
  return term

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

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

pKeyword :: Text -> Parser Text
pKeyword keyword = lexeme (string keyword)

pKeyword' :: Text -> Parser Text
pKeyword' keyword = lexeme (string' keyword <* notFollowedBy alphaNumChar)

parseText :: Parser Text
parseText = T.pack <$> lexeme (some letterChar)

parseReturn :: Parser QueryExpr
parseReturn = do
  symbol' "RETURN"
  return Return

parseMatch :: Parser (QueryExpr -> QueryExpr)
parseMatch = do
  sc
  symbol' "MATCH"
  node <- manyTill
    (choice
       [ parens parseNode <?> "valid node"
       , brackets parseRelationship <?> "valid relationship"
       , parseConnectorDirection <?> "connector"])
    -- Might need to update this to lookahead to something more intelligent
    (lookAhead . choice $ [pKeyword' "RETURN", pKeyword' "MATCH", symbol ","])
  return $ Match node

parseNode :: Parser MatchSection
parseNode = Node <$> optional parseText <*> (symbol ":" *> parseText)

parseRelationship :: Parser MatchSection
parseRelationship = Relationship <$> optional parseText
  <*> (symbol ":" *> parseText)

parseConnectorDirection :: Parser MatchSection
parseConnectorDirection = ConnectorDirection
  <$> choice
    [ RightDirection <$ symbol "->"
    , NoDirection <$ symbol "-"
    , LeftDirection <$ symbol "<-"]