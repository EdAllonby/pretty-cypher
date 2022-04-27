module Cypher.QuasiQuoteSpec (runQuasiQuoteTests) where

import Cypher.QuasiQuote (cypher)
import Cypher.Types
import Data.Map qualified as M
import Test.Hspec

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
  RETURN *
|]

runQuasiQuoteTests :: SpecWith ()
runQuasiQuoteTests = describe "Cypher.QuasiQuote" $
  do
    context "when running cypher quasiquote" $
      do
        it "correctly parses the DSL" $
          do
            cypherExpr
              `shouldBe` [ Match
                             [ MatchPattern
                                 Nothing
                                 [ Node
                                     ( LabelledPatternComponentType
                                         (Just (UnboundText "per"))
                                         [UnboundText "Person"]
                                     )
                                     ( M.fromList
                                         [ (UnboundText "age", IntegerValue 32),
                                           ( UnboundText "base",
                                             DoubleValue (-3.14)
                                           ),
                                           ( UnboundText "delta",
                                             IntegerValue (-10)
                                           ),
                                           ( UnboundText "height",
                                             DoubleValue 1.6
                                           ),
                                           ( UnboundText "name",
                                             TextValue
                                               (QuotedText " D. A. V. E ")
                                           )
                                         ]
                                     )
                                 ]
                             ],
                           Return False ReturnAllElements
                         ]