
module Types where

import           Data.Text (Text)
import qualified Data.Map as M
import           Data.Data (Data, Typeable)

data ConnectorDirection = LeftDirection
                        | RightDirection
                        | NoDirection
  deriving (Data, Typeable, Eq, Show)

data PropertyValue = TextValue Text
                   | IntegerValue Integer
                   | DoubleValue Double
  deriving (Data, Typeable, Eq, Show)

type Properties = M.Map Text PropertyValue

data Node = LabelledNode { labelledNodeVariable :: Maybe Text
                         , labelledNodeLabel :: Text
                         , labelledNodeProperties :: Properties
                         }
          | AnyNode { anyNodeVariable :: Text }
          | EmptyNode
  deriving (Data, Typeable, Eq, Show)

data Relationship =
    LabelledRelationship { labelledRelationshipVariable :: Maybe Text
                         , labelledRelationshipLabel :: Text
                         , labelledRelationshipProperties :: Properties
                         }
  | AnyRelationship { anyRelationshipVariable :: Text }
  deriving (Data, Typeable, Eq, Show)

data Pattern = Node Node
             | Relationship Relationship
             | ConnectorDirection ConnectorDirection
  deriving (Data, Typeable, Eq, Show)

data QueryExpr = Match [Pattern] QueryExpr
               | Return
  deriving (Data, Typeable, Eq, Show)
