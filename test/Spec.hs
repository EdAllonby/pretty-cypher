import           Test.Hspec (hspec)
import           Cypher.Parser.QuerySpec (runParserQueryTests)
import           Cypher.Parser.PatternSpec (runParserPatternTests)
import           Cypher.Parser.MatchSpec (runParserMatchTests)
import           Cypher.Parser.ReturnSpec (runParserReturnTests)
import           Cypher.QuasiQuoteSpec (runQuasiQuoteTests)

main :: IO ()
main = hspec
  $ do
    runParserQueryTests
    runParserPatternTests
    runParserMatchTests
    runParserReturnTests
    runQuasiQuoteTests