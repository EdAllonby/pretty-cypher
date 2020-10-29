module Parser.Query (parseQuery) where

import           Types (QueryExpr)
import           Parser.ParserCore (Parser, sc)
import           Text.Megaparsec ((<?>), choice, manyTill, MonadParsec(eof))
import           Parser.Match (parseMatch, parseOptionalMatch)
import           Parser.Return (parseReturn)

parseQuery :: Parser QueryExpr
parseQuery = sc
  *> manyTill
    (choice
       [ parseMatch <?> "match clause"
       , parseOptionalMatch <?> "optional match clause"
       , parseReturn <?> "return clause"])
    eof
