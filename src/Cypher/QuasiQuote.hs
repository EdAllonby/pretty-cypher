module Cypher.QuasiQuote (cypher) where

import qualified Data.Text as T
import           Language.Haskell.TH.Quote (QuasiQuoter(..), dataToExpQ)
import           Language.Haskell.TH (Exp(VarE, AppE))
import           Language.Haskell.TH.Syntax (lift)
import qualified Text.Megaparsec.Error as ME
import qualified Text.Megaparsec as M
import           Data.Data
import           Cypher.Parser.Query (parseQuery)

cypher :: QuasiQuoter
cypher =
  QuasiQuoter { quoteExp = \unparsedCypher -> do
                  parsedQuery
                    <- case M.parse parseQuery "" (T.pack unparsedCypher) of
                      Left
                        e -> fail $ ME.errorBundlePretty e
                      Right parsedQuery -> return parsedQuery
                  dataToExpQ
                    (fmap ((AppE (VarE 'T.pack) <$>) . lift . T.unpack) . cast)
                    parsedQuery
              , quotePat = undefined
              , quoteType = undefined
              , quoteDec = undefined
              }