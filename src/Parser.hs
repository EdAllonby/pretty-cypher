module Parser (parseQuery) where

import           Types
import           ParserCore
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.Megaparsec
import           Text.Megaparsec.Char

parseQuery :: Parser QueryExpr
parseQuery = sc
  *> choice
    [ (parseMatch <*> parseQuery) <?> "match clause"
    , parseReturn <?> "return clause"]
  <* eof

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

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
  symbol' "MATCH"
  node <- manyTill
    (choice
       [ Node <$> parseNode <?> "valid node"
       , Relationship <$> parseRelationship <?> "valid relationship"
       , parseConnectorDirection <?> "connector"])
    -- Might need to update this lookahead to something more intelligent
    (lookAhead . choice $ [pKeyword' "RETURN", pKeyword' "MATCH", symbol ","])
  return $ Match node

parseNode :: Parser Node
parseNode = parens
  $ choice
    [ try $ LabelledNode <$> optional parseText <*> (symbol ":" *> parseText)
    , AnyNode <$> parseText
    , EmptyNode <$ symbol ""]

parseRelationship :: Parser Relationship
parseRelationship = brackets
  $ choice
    [ try
        $ LabelledRelationship <$> optional parseText
        <*> (symbol ":" *> parseText)
    , AnyRelationship <$> parseText]

parseConnectorDirection :: Parser MatchSection
parseConnectorDirection = ConnectorDirection
  <$> choice
    [ RightDirection <$ symbol "->"
    , NoDirection <$ symbol "-"
    , LeftDirection <$ symbol "<-"]