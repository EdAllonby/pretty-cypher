
module Types where

import           Data.Text (Text)
import qualified Data.Map as M
import           Data.Data (Data, Typeable)

data ConnectorDirection =
    LeftDirection
  | AnonymousLeftDirection
  | RightDirection
  | AnonymousRightDirection
  | NoDirection
  | AnonymousNoDirection
  deriving (Data, Typeable, Eq, Show)

data PropertyValue = TextValue Text
                   | IntegerValue Integer
                   | DoubleValue Double
  deriving (Data, Typeable, Eq, Show)

type Properties = M.Map Text PropertyValue

data NodeType = LabelledNode { labelledNodeVariable :: Maybe Text
                             , labelledNodeLabel :: Text
                             }
              | AnyNode { anyNodeVariable :: Text }
              | EmptyNode
  deriving (Data, Typeable, Eq, Show)

data RelationshipType =
    LabelledRelationship { labelledRelationshipVariable :: Maybe Text
                         , labelledRelationshipLabel :: Text
                         }
  | AnyRelationship { anyRelationshipVariable :: Text }
  | EmptyRelationship
  deriving (Data, Typeable, Eq, Show)

data PatternComponent = Node NodeType Properties
                      | Relationship RelationshipType Properties
                      | ConnectorDirection ConnectorDirection
  deriving (Data, Typeable, Eq, Show)

type Pattern = [PatternComponent]

data Clause = Match [Pattern] -- TODO: Move this to a non-empty list data type?
            | Return
  deriving (Data, Typeable, Eq, Show)

type QueryExpr = [Clause]
