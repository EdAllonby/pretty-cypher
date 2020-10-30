import           Test.Hspec (hspec)
import           Parser.QuerySpec (runParserQueryTests)
import           Parser.MatchSpec (runParserMatchTests)
import           Parser.ReturnSpec (runParserReturnTests)
import           QuasiQuoteSpec (runQuasiQuoteTests)

main :: IO ()
main = hspec
  $ do
    runParserQueryTests
    runParserMatchTests
    runParserReturnTests
    runQuasiQuoteTests