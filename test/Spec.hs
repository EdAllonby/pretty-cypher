
import           Test.Hspec (hspec)
import           Parser.QuerySpec (runParserQueryTests)
import           Parser.MatchSpec (runParserMatchTests)
import           QuasiQuoteSpec (runQuasiQuoteTests)

main :: IO ()
main = hspec
  $ do
    runParserQueryTests
    runParserMatchTests
    runQuasiQuoteTests