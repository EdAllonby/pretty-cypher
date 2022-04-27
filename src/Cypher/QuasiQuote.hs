module Cypher.QuasiQuote (cypher) where

import Cypher.Parser.Query (parseQuery)
import Data.Data
import Data.Text qualified as T
import Language.Haskell.TH (Exp (AppE, VarE))
import Language.Haskell.TH.Quote (QuasiQuoter (..), dataToExpQ)
import Language.Haskell.TH.Syntax (lift)
import Text.Megaparsec qualified as M
import Text.Megaparsec.Error qualified as ME

cypher :: QuasiQuoter
cypher =
  QuasiQuoter
    { quoteExp = \unparsedCypher -> do
        parsedQuery <-
          case M.parse parseQuery "" (T.pack unparsedCypher) of
            Left
              e -> fail $ ME.errorBundlePretty e
            Right parsedQuery -> return parsedQuery
        dataToExpQ
          (fmap ((AppE (VarE 'T.pack) <$>) . lift . T.unpack) . cast)
          parsedQuery,
      quotePat = undefined,
      quoteType = undefined,
      quoteDec = undefined
    }