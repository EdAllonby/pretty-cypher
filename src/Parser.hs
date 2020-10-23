module Parser (parseQuery) where

import           Types
import           ParserCore
import           Data.Text (Text)
import           Text.Megaparsec
import qualified Data.Map as M

parseQuery :: Parser QueryExpr
parseQuery = sc
  *> choice
    [ (parseMatch <*> parseQuery) <?> "match clause"
    , parseReturn <?> "return clause"]
  <* eof

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
    (lookAhead . choice $ [keyword' "RETURN", keyword' "MATCH", symbol ","])
  return $ Match node

parseNode :: Parser Node
parseNode = parens
  $ choice
    [ try
        $ LabelledNode <$> optional parseText
        <*> (symbol ":" *> parseText)
        <*> (parseProperties <?> "labels")
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

parseProperties :: Parser (M.Map Text Text)
parseProperties = do
  props <- optional $ curlyBrackets $ parseProperty `sepBy` symbol ","
  return $ M.unions (M.fromList <$> props)

parseProperty :: Parser (Text, Text)
parseProperty = (,) <$> parseText <*> (symbol ":" *> betweenQuotes)