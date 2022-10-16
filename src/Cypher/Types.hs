module Cypher.Types where

import Data.Data (Data, Typeable)
import Data.Map qualified as M
import Data.Text (Text)

data LiteralText
  = QuotedText Text
  | BacktickedText Text
  | UnboundText Text
  deriving (Data, Ord, Typeable, Eq, Show)

data ConnectorDirection
  = LeftDirection
  | AnonymousLeftDirection
  | RightDirection
  | AnonymousRightDirection
  | NoDirection
  | AnonymousNoDirection
  deriving (Data, Typeable, Eq, Show)

data PropertyValue
  = TextValue LiteralText
  | IntegerValue Integer
  | DoubleValue Double
  | BooleanValue Bool
  | ObjectValue Object
  deriving (Data, Typeable, Eq, Show)

type Properties = M.Map LiteralText PropertyValue

data RelationshipHops
  = VariableHops Integer Integer
  | MinHops Integer
  | MaxHops Integer
  | FixedHops Integer
  | AnyHops
  deriving (Data, Typeable, Eq, Show)

data PatternComponentType
  = LabelledPatternComponentType
      { labelledVariable :: Maybe LiteralText,
        labelledLabels :: [LiteralText]
      }
  | AnyPatternComponentType {anyVariable :: LiteralText}
  | EmptyPatternComponentType
  deriving (Data, Typeable, Eq, Show)

data PatternComponent
  = Node PatternComponentType Properties
  | Relationship PatternComponentType (Maybe RelationshipHops) Properties
  | ConnectorDirection ConnectorDirection
  deriving (Data, Typeable, Eq, Show)

data Function a = Function {functionName :: Text, functionContents :: a}
  deriving (Data, Typeable, Eq, Show)

type Pattern = [PatternComponent]

data Object
  = NestedObject LiteralText Object
  | ObjectEnd
  deriving (Data, Typeable, Eq, Show)

data Property = Property
  { propertyValue :: PropertyValue,
    propertyAlias :: Maybe LiteralText
  }
  deriving (Data, Typeable, Eq, Show)

data ReturnExpression
  = ReturnProperty Property
  | ReturnPattern Pattern
  | ReturnFunctionWrappedPattern (Function Pattern)
  | ReturnFunctionWrappedPropertyWithArity (Function [PropertyValue])
  deriving (Data, Typeable, Eq, Show)

data ReturnValue
  = ReturnExpressions [ReturnExpression]
  | ReturnAllElements
  deriving (Data, Typeable, Eq, Show)

data MatchValue
  = MatchFunctionWrappedPattern
      { functionWrappedPatternVariable :: Maybe Text,
        functionWrappedPattern :: Function Pattern
      }
  | MatchPattern
      { matchPatternVariable :: Maybe Text,
        matchPattern :: Pattern
      }
  deriving (Data, Typeable, Eq, Show)

data WithValue
  = WithWildcard
  | WithProperty Property
  | WithFunctionWrappedProperty (Function Property)
  deriving (Data, Typeable, Eq, Show)

data Clause
  = Match [MatchValue] -- TODO: Move this to a non-empty list data type?
  | OptionalMatch [MatchValue]
  | With [WithValue]
  | Create [Pattern]
  | Delete [LiteralText] -- TODO: Can also be literal text surrounded by brackets, i.e. Delete (n). Doesn't mean node.
  | DetachDelete [LiteralText]
  | Return {isDistinct :: Bool, returnValue :: ReturnValue}
  | Noop
  deriving (Data, Typeable, Eq, Show)

type QueryExpr = [Clause]
