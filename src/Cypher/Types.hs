
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
                   | ObjectValue Object
  deriving (Data, Typeable, Eq, Show)

type Properties = M.Map LiteralText PropertyValue

data RelationshipHops = VariableHops Integer Integer
                      | MinHops Integer
                      | MaxHops Integer
                      | FixedHops Integer
                      | AnyHops
  deriving (Data, Typeable, Eq, Show)

data PatternComponentType =
    LabelledPatternComponentType { labelledVariable :: Maybe LiteralText
                                 , labelledLabels :: [LiteralText]
                                 }
  | AnyPatternComponentType { anyVariable :: LiteralText }
  | EmptyPatternComponentType
  deriving (Data, Typeable, Eq, Show)

data PatternComponent =
    Node PatternComponentType Properties
  | Relationship PatternComponentType (Maybe RelationshipHops) Properties
  | ConnectorDirection ConnectorDirection
  deriving (Data, Typeable, Eq, Show)

data Function a = Function { functionName :: Text, functionContents :: a }
  deriving (Data, Typeable, Eq, Show)

type Pattern = [PatternComponent]

data Object = NestedObject LiteralText Object
            | ObjectEnd
  deriving (Data, Typeable, Eq, Show)

data ReturnProperty = Property { propertyValue :: PropertyValue
                               , propertyAlias :: Maybe LiteralText
                               }
  deriving (Data, Typeable, Eq, Show)

data ReturnExpression =
    ReturnProperty ReturnProperty
  | ReturnPattern Pattern
  | ReturnFunctionWrappedPattern (Function Pattern)
  | ReturnFunctionWrappedPropertyWithArity (Function [PropertyValue])
  deriving (Data, Typeable, Eq, Show)

data ReturnValue = ReturnExpressions [ReturnExpression]
                 | ReturnAllElements
  deriving (Data, Typeable, Eq, Show)

data MatchValue =
    MatchFunctionWrappedPattern { functionWrappedPatternVariable :: Maybe Text
                                , functionWrappedPattern :: Function Pattern
                                }
  | MatchPattern { matchPatternVariable :: Maybe Text
                 , matchPattern :: Pattern
                 }
  deriving (Data, Typeable, Eq, Show)

data Clause =
    Match [MatchValue] -- TODO: Move this to a non-empty list data type?
  | OptionalMatch [MatchValue]
  | Create [Pattern]
  | Delete [LiteralText] -- TODO: Can also be literal text surrounded by brackets, i.e. Delete (n). Doesn't mean node.
  | DetachDelete [LiteralText]
  | Return { isDistinct :: Bool, returnValue :: ReturnValue }
  deriving (Data, Typeable, Eq, Show)

type QueryExpr = [Clause]
