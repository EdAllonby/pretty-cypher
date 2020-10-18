module Parser where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec, parseTest, some)
import Text.Megaparsec.Char (char, letterChar, lowerChar, spaceChar, string')

x = 1

data Node = Node {alias :: Char, name :: String} deriving (Show)

type Parser = Parsec Void Text

exampleQuery :: Text
exampleQuery = "mATCH (u:User)-[r:Relationship]->(u:User)"

runParse :: IO ()
runParse = parseTest parseQuery exampleQuery

parseMatch :: Parser Node
parseMatch = do
  string' "MATCH"
  some spaceChar
  node <- char '(' *> parseNode <* char ')'
  return node

parseQuery :: Parser Node
parseQuery = do
  node <- parseMatch
  return (node)

parseNode :: Parser Node
parseNode = do
  alias <- lowerChar
  char ':'
  name <- some letterChar
  return (Node alias name)