module Parser.Match (parseMatch, parseOptionalMatch) where

import           Types (Clause(OptionalMatch, Match), Pattern(Pattern)
                      , PatternComponent(ConnectorDirection, Node, Relationship)
                      , RelationshipType(EmptyRelationship, LabelledRelationship,
                 AnyRelationship)
                      , NodeType(EmptyNode, LabelledNode, AnyNode)
                      , PropertyValue(..), ConnectorDirection(..))
import           Parser.ParserCore (Parser, symbol, signedInteger, signedDouble
                                  , keyword', parens, brackets, curlyBrackets
                                  , parseText, parseSnakeCaseText, betweenQuotes
                                  , commaSep)
import           Data.Text (Text)
import           Text.Megaparsec (eof, sepBy1, optional, (<?>), choice, manyTill
                                , MonadParsec(try, lookAhead))
import qualified Data.Map as M

parseMatch :: Parser Clause
parseMatch = do
  keyword' "MATCH"
  Match <$> commaSep parsePattern

parseOptionalMatch :: Parser Clause
parseOptionalMatch = do
  keyword' "OPTIONAL MATCH"
  OptionalMatch <$> commaSep parsePattern

parsePattern :: Parser Pattern
parsePattern = do
  patternVariable <- optional $ parseText <* symbol "="
  patternComponents <- manyTill
    (choice
       [ parens (Node <$> parseNodeType <*> parseProperties) <?> "valid node"
       , brackets (Relationship <$> parseRelationshipType <*> parseProperties)
           <?> "valid relationship"
       , ConnectorDirection <$> parseConnectorDirection <?> "connector"])
    -- Can we unify these lookahead tokens with those specified in the above parseQuery table?
    (lookAhead . choice
     $ [ () <$ keyword' "RETURN"
       , () <$ keyword' "MATCH"
       , () <$ keyword' "OPTIONAL MATCH"
       , () <$ symbol ","
       , eof])
  return $ Pattern patternVariable patternComponents

parseNodeType :: Parser NodeType
parseNodeType = choice
  [ try
      $ LabelledNode <$> optional parseText
      <*> (symbol ":" *> parseText `sepBy1` symbol ":")
  , AnyNode <$> parseText
  , EmptyNode <$ symbol ""]

parseRelationshipType :: Parser RelationshipType
parseRelationshipType = choice
  [ try
      $ LabelledRelationship <$> optional parseText
      <*> (symbol ":" *> parseSnakeCaseText)
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
  props <- optional $ curlyBrackets $ commaSep parseProperty
  return $ M.unions (M.fromList <$> props)

parseProperty :: Parser (Text, PropertyValue)
parseProperty = (,) <$> parseText
  <*> (symbol ":"
       *> choice
         [ DoubleValue <$> try signedDouble -- TODO: Not too sure why this one needs a try, shouldn't it be atomic? Investigate
         , IntegerValue <$> signedInteger
         , TextValue <$> betweenQuotes])