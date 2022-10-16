module Cypher.Parser.Query (parseQuery) where

import Cypher.Parser.Core (Parser, sc)
import Cypher.Parser.Create (parseCreate)
import Cypher.Parser.Delete (parseDelete, parseDetachedDelete)
import Cypher.Parser.Match (parseMatch, parseOptionalMatch)
import Cypher.Parser.Return (parseReturn)
import Cypher.Parser.With (parseWith)
import Cypher.Types (QueryExpr)
import Text.Megaparsec (MonadParsec (eof), choice, manyTill, (<?>))

parseQuery :: Parser QueryExpr
parseQuery =
  sc
    *> manyTill
      ( choice
          [ parseMatch <?> "match clause",
            parseOptionalMatch <?> "optional match clause",
            parseWith <?> "with clause",
            parseCreate <?> "create clause",
            parseDelete <?> "delete clause",
            parseDetachedDelete <?> "detach delete clause",
            parseReturn <?> "return clause"
          ]
      )
      eof
