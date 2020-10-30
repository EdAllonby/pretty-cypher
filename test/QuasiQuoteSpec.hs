{-# LANGUAGE QuasiQuotes #-}

module QuasiQuoteSpec where

import           Test.Hspec
import           QuasiQuote (cypher)
import qualified Data.Map as M
import           Types

-- Just the fact that this quasi-quote compiles is enough to show the module is working, 
-- but let's document this with a test.
cypherExpr :: QueryExpr
cypherExpr =
  [cypher|
  // This is an example of a comment in cypher
  MATCH (per:Person {
    name: ' D. A. V. E ', 
    age: 32, height: 1.6, // This is a some more comments
    delta: -10, 
    base: -3.14 
  })
  RETURN
|]

runQuasiQuoteTests :: SpecWith ()
runQuasiQuoteTests = describe "QuasiQuote"
  $ do
    context "when running cypher quasiquote"
      $ do
        it "correctly parses the DSL"
          $ do
            cypherExpr
              `shouldBe` [ Match
                             [ Pattern
                                 Nothing
                                 Nothing
                                 [ Node
                                     (LabelledNode (Just "per") ["Person"])
                                     (M.fromList
                                        [ ("age", IntegerValue 32)
                                        , ("base", DoubleValue (-3.14))
                                        , ("delta", IntegerValue (-10))
                                        , ("height", DoubleValue 1.6)
                                        , ("name", TextValue " D. A. V. E ")])]]
                         , Return]