import Cypher.Parser.CreateSpec (runParserCreateTests)
import Cypher.Parser.DeleteSpec (runParserDeleteTests)
import Cypher.Parser.MatchSpec (runParserMatchTests)
import Cypher.Parser.PatternSpec (runParserPatternTests)
import Cypher.Parser.QuerySpec (runParserQueryTests)
import Cypher.Parser.ReturnSpec (runParserReturnTests)
import Cypher.QuasiQuoteSpec (runQuasiQuoteTests)
import Test.Hspec (hspec)

main :: IO ()
main = hspec $
  do
    runParserQueryTests
    runParserPatternTests
    runParserMatchTests
    runParserCreateTests
    runParserDeleteTests
    runParserReturnTests
    runQuasiQuoteTests
