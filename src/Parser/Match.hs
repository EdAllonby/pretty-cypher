module Parser.Match (parseMatch, parseOptionalMatch, parseRelationshipHops) where

import           Types (RelationshipHops(..), Clause(OptionalMatch, Match)
                      , Pattern(Pattern)
                      , PatternComponent(ConnectorDirection, Node, Relationship)
                      , RelationshipType(EmptyRelationship, LabelledRelationship,
                 AnyRelationship)
                      , NodeType(EmptyNode, LabelledNode, AnyNode)
                      , PropertyValue(..), ConnectorDirection(..))
import           Parser.ParserCore (boolean, integer, Parser, symbol
                                  , signedInteger, signedDouble, keyword'
                                  , parens, brackets, curlyBrackets, parseText
                                  , parseSnakeCaseText, betweenQuotes, commaSep)
import           Data.Text (Text)
import           Text.Megaparsec ((<|>), eof, sepBy1, optional, (<?>), choice
                                , manyTill, MonadParsec(try, lookAhead))
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
       , eof]) -- TODO: Really don't want to have this EOF, but we have it here otherwise we need to add RETURN statements to tests. Is there another option?
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
      <*> (symbol ":"
           *> parseSnakeCaseText `sepBy1` (symbol "|:" <|> symbol "|")) -- TODO: It doesn't really matter if this is snake case, we need a more general text parser.
  , AnyRelationship <$> parseText
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
         , BooleanValue <$> boolean
         , TextValue <$> betweenQuotes])