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
  keyword' "RETURN"
  return Return

parseMatch :: Parser (QueryExpr -> QueryExpr)
parseMatch = do
  keyword' "MATCH"
  node <- manyTill
    (choice
       [ parens (Node <$> parseNodeType <*> parseProperties) <?> "valid node"
       , Relationship <$> parseRelationship <?> "valid relationship"
       , ConnectorDirection <$> parseConnectorDirection <?> "connector"])
    -- Might need to update this lookahead to something more intelligent
    (lookAhead . choice $ [keyword' "RETURN", keyword' "MATCH", symbol ","])
  return $ Match node

parseNodeType :: Parser NodeType
parseNodeType = choice
  [ try $ LabelledNode <$> optional parseText <*> (symbol ":" *> parseText)
  , AnyNode <$> parseText
  , EmptyNode <$ symbol ""]

parseRelationship :: Parser Relationship
parseRelationship = brackets
  $ choice
    [ try
        $ LabelledRelationship <$> optional parseText
        <*> (symbol ":" *> parseText)
        <*> parseProperties
    , AnyRelationship <$> parseText
    , EmptyRelationship <$ symbol ""]

parseConnectorDirection :: Parser ConnectorDirection
parseConnectorDirection = choice
  [ AnonymousRightDirection <$ symbol "-->"
  , RightDirection <$ symbol "->"
  , AnonymousNoDirection <$ symbol "--"
  , NoDirection <$ symbol "-"
  , AnonymousLeftDirection <$ symbol "<--"
  , LeftDirection <$ symbol "<-"]

parseProperties :: Parser (M.Map Text PropertyValue)
parseProperties = do
  props <- optional $ curlyBrackets $ parseProperty `sepBy` symbol ","
  return $ M.unions (M.fromList <$> props)

parseProperty :: Parser (Text, PropertyValue)
parseProperty = (,) <$> parseText
  <*> (symbol ":"
       *> choice
         [ DoubleValue <$> try signedDouble -- TODO: Not too sure why this one needs a try, shouldn't it be atomic? Investigate
         , IntegerValue <$> signedInteger
         , TextValue <$> betweenQuotes])