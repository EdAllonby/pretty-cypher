module Types where

import           Data.Text (Text)

data ConnectorDirection = LeftDirection
                        | RightDirection
                        | NoDirection
  deriving (Show, Eq)

data Node = LabelledNode { labelledNodeVariable :: Maybe Text
                         , labelledNodeLabel :: Text
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
