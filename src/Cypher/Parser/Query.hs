module Cypher.Parser.Query (parseQuery) where

import           Cypher.Types (QueryExpr)
import           Cypher.Parser.Core (Parser, sc)
import           Text.Megaparsec ((<?>), choice, manyTill, MonadParsec(eof))
import           Cypher.Parser.Match (parseMatch, parseOptionalMatch)
import           Cypher.Parser.Return (parseReturn)

parseQuery :: Parser QueryExpr
parseQuery = sc
  *> manyTill
    (choice
       [ parseMatch <?> "match clause"
       , parseOptionalMatch <?> "optional match clause"
       , parseReturn <?> "return clause"])
    eof
