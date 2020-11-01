-- TODO: HLS / Floskell doesn't seem to respect the default-extensions in package.yaml. 
-- Need to explicitely provide this at the top of file.
-- See https://github.com/ennocramer/floskell/issues/39
{-# LANGUAGE TupleSections #-}

module Cypher.Parser.Pattern (parsePattern) where

import           Cypher.Types (Pattern, PatternComponent(ConnectorDirection, Node, Relationship)
                             , RelationshipType(EmptyRelationship, LabelledRelationship,
                 AnyRelationship)
                             , NodeType(EmptyNode, LabelledNode, AnyNode)
                             , RelationshipHops(..), PropertyValue(..)
                             , ConnectorDirection(..), LiteralText)
import           Cypher.Parser.Core (parseLiteralText, boolean, integer, Parser
                                   , symbol, signedInteger, signedDouble
                                   , keyword', parens, brackets, curlyBrackets
                                   , commaSep)
import           Text.Megaparsec ((<|>), eof, sepBy1, optional, (<?>), choice
                                , manyTill, MonadParsec(try, lookAhead))
import qualified Data.Map as M

parsePattern :: Parser Pattern
parsePattern = manyTill
  (choice
     [ parens (Node <$> parseNodeType <*> parseProperties) <?> "valid node"
     , brackets
         (Relationship <$> parseRelationshipType
          <*> optional parseRelationshipHops
          <*> parseProperties)
         <?> "valid relationship"
     , ConnectorDirection <$> parseConnectorDirection <?> "connector"])
  -- Can we unify these lookahead tokens with those specified in the above parseQuery table?
  (lookAhead . choice
   $ [ () <$ keyword' "RETURN"
     , () <$ keyword' "MATCH"
     , () <$ keyword' "OPTIONAL MATCH"
     , () <$ symbol ","
     , () <$ symbol ")"
     , eof]) -- TODO: Really don't want to have this EOF, but we have it here otherwise we need to add RETURN statements to tests. Is there another option?

parseNodeType :: Parser NodeType
parseNodeType = choice
  [ try
      $ LabelledNode <$> optional parseLiteralText
      <*> (symbol ":" *> parseLiteralText `sepBy1` symbol ":")
  , AnyNode <$> parseLiteralText
  , EmptyNode <$ symbol ""]

parseRelationshipType :: Parser RelationshipType
parseRelationshipType = choice
  [ try
      $ LabelledRelationship <$> optional parseLiteralText
      <*> (symbol ":" *> parseLiteralText `sepBy1` (symbol "|:" <|> symbol "|"))
  , AnyRelationship <$> parseLiteralText
  , EmptyRelationship <$ symbol ""]

parseRelationshipHops :: Parser RelationshipHops
parseRelationshipHops = symbol "*"
  *> choice
    [ try (VariableHops <$> integer <*> (symbol ".." *> integer))
    , try (MinHops <$> (integer <* symbol ".."))
    , MaxHops <$> (symbol ".." *> integer)
    , FixedHops <$> integer
    , return AnyHops]

parseConnectorDirection :: Parser ConnectorDirection
parseConnectorDirection = choice
  [ AnonymousRightDirection <$ symbol "-->"
  , RightDirection <$ symbol "->"
  , AnonymousNoDirection <$ symbol "--"
  , NoDirection <$ symbol "-"
  , AnonymousLeftDirection <$ symbol "<--"
  , LeftDirection <$ symbol "<-"]

parseProperties :: Parser (M.Map LiteralText PropertyValue)
parseProperties = do
  props <- optional $ curlyBrackets $ commaSep parseProperty
  return $ M.unions (M.fromList <$> props)

parseProperty :: Parser (LiteralText, PropertyValue)
parseProperty = (,) <$> parseLiteralText
  <*> (symbol ":"
       *> choice
         [ DoubleValue <$> try signedDouble -- TODO: Not too sure why this one needs a try, shouldn't it be atomic? Investigate
         , IntegerValue <$> signedInteger
         , BooleanValue <$> boolean
         , TextValue <$> parseLiteralText])
