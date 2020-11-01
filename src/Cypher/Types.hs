
module Cypher.Types where

import           Data.Text (Text)
import qualified Data.Map as M
import           Data.Data (Data, Typeable)

data LiteralText = QuotedText Text
                 | BacktickedText Text
                 | UnboundText Text
  deriving (Data, Ord, Typeable, Eq, Show)

data ConnectorDirection =
    LeftDirection
  | AnonymousLeftDirection
  | RightDirection
  | AnonymousRightDirection
  | NoDirection
  | AnonymousNoDirection
  deriving (Data, Typeable, Eq, Show)

data PropertyValue = TextValue LiteralText
                   | IntegerValue Integer
                   | DoubleValue Double
                   | BooleanValue Bool
  deriving (Data, Typeable, Eq, Show)

type Properties = M.Map LiteralText PropertyValue

data RelationshipHops = VariableHops Integer Integer
                      | MinHops Integer
                      | MaxHops Integer
                      | FixedHops Integer
                      | AnyHops
  deriving (Data, Typeable, Eq, Show)

data NodeType = LabelledNode { labelledNodeVariable :: Maybe LiteralText
                             , labelledNodeLabels :: [LiteralText]
                             }
              | AnyNode { anyNodeVariable :: LiteralText }
              | EmptyNode
  deriving (Data, Typeable, Eq, Show)

data RelationshipType =
    LabelledRelationship { labelledRelationshipVariable :: Maybe LiteralText
                         , labelledRelationshipLabel :: [LiteralText]
                         }
  | AnyRelationship { anyRelationshipVariable :: LiteralText }
  | EmptyRelationship
  deriving (Data, Typeable, Eq, Show)

data PatternComponent =
    Node NodeType Properties
  | Relationship RelationshipType (Maybe RelationshipHops) Properties
  | ConnectorDirection ConnectorDirection
  deriving (Data, Typeable, Eq, Show)

data Pattern = Pattern { patternVariable :: Maybe Text
                       , patternWrappingFunctionName :: Maybe Text
                       , patternComponents :: [PatternComponent]
                       }
  deriving (Data, Typeable, Eq, Show)

data Object = NestedObject LiteralText Object
            | ObjectEnd
  deriving (Data, Typeable, Eq, Show)

data ReturnProperty =
  Property { propertyObject :: Object, propertyAlias :: Maybe LiteralText }
  deriving (Data, Typeable, Eq, Show)

data ReturnExpression = ReturnProperty ReturnProperty
                      | ReturnPattern Pattern
  deriving (Data, Typeable, Eq, Show)

data ReturnValue = ReturnExpressions [ReturnExpression]
                 | ReturnAllElements
  deriving (Data, Typeable, Eq, Show)

data Clause = Match [Pattern] -- TODO: Move this to a non-empty list data type?
            | OptionalMatch [Pattern]
            | Return ReturnValue
  deriving (Data, Typeable, Eq, Show)

type QueryExpr = [Clause]
