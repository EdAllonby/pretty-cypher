module Parser.ReturnSpec where

import           Types
import           Parser.Return
import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec
import           Data.Text as T

runParserReturnTests :: SpecWith ()
runParserReturnTests = describe "Parser.Return"
  $ do
    context "when parsing return query"
      $ do
        context "with standard clause" runStandardParserReturnTests

runStandardParserReturnTests = do
  it "parses return clause"
    $ "RETURN n" `shouldParseReturnQuery` Return (UnboundText "n")

shouldParseReturnQuery :: Text -> Clause -> Expectation
shouldParseReturnQuery query expectedResult =
  parse parseReturn "" query `shouldParse` expectedResult
