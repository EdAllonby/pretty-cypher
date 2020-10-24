
import           Test.Hspec
import           ParserSpec
import           QuasiQuoteSpec

main :: IO ()
main = hspec
  $ do
    runParserTests
    runQuasiQuoteTests