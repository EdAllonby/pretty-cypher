module Types where

import           Data.Text (Text)
import qualified Data.Map as M

data ConnectorDirection = LeftDirection
                        | RightDirection
                        | NoDirection
  deriving (Show, Eq)

type Properties = M.Map Text Text

data Node = LabelledNode { labelledNodeVariable :: Maybe Text
                         , labelledNodeLabel :: Text
                         , properties :: Properties
                         }
          | AnyNode { anyNodeVariable :: Text }
          | EmptyNode
  deriving (Show, Eq)

data Relationship =
    LabelledRelationship { labelledRelationshipVariable :: Maybe Text
                         , labelledRelationshipLabel :: Text
                         }
  | AnyRelationship { anyRelationshipVariable :: Text }
  deriving (Show, Eq)

data MatchSection = Node Node
                  | Relationship Relationship
                  | ConnectorDirection ConnectorDirection
  deriving (Show, Eq)

data QueryExpr = Match [MatchSection] QueryExpr
               | Return
  deriving (Eq, Show)
