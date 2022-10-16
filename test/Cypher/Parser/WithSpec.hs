module Cypher.Parser.WithSpec (runParserWithTests) where

import Cypher.Parser.With (parseWith)
import Cypher.Types
import Data.Text as T (Text)
import Test.Hspec (Expectation, SpecWith, describe, it)
import Test.Hspec.Megaparsec (shouldParse)
import Text.Megaparsec (parse)

runParserWithTests :: SpecWith ()
runParserWithTests = describe "Cypher.Parser.With" $
  do
    it "parses wildcard" $
      "WITH *"
        `shouldParseWithQuery` With [WithWildcard]
    it "parses single with clause with single literal text" $
      "WITH a"
        `shouldParseWithQuery` With [WithProperty (Property (TextValue (UnboundText "a")) Nothing)]
    it "parses single with clause with single literal aliased text" $
      "WITH a as Person"
        `shouldParseWithQuery` With [WithProperty (Property (TextValue (UnboundText "a")) (Just (UnboundText "Person")))]
    it "parses single with clause with multiple literal texts" $
      "WITH a, `b`"
        `shouldParseWithQuery` With
          [ WithProperty (Property (TextValue (UnboundText "a")) Nothing),
            WithProperty (Property (TextValue (BacktickedText "b")) Nothing)
          ]
    it "parses function" $
      "WITH toUpper(otherPerson.name)"
        `shouldParseWithQuery` With
          [ WithFunctionWrappedProperty
              ( Function
                  { functionName = "toUpper",
                    functionContents =
                      Property
                        { propertyValue = ObjectValue (NestedObject (UnboundText "otherPerson") (NestedObject (UnboundText "name") ObjectEnd)),
                          propertyAlias = Nothing
                        },
                    functionAlias = Nothing
                  }
              )
          ]
    it "parses function with alias" $
      "WITH otherPerson, toUpper(otherPerson.name) AS upperCaseName"
        `shouldParseWithQuery` With
          [ WithProperty
              Property
                { propertyValue = TextValue (UnboundText "otherPerson"),
                  propertyAlias = Nothing
                },
            WithFunctionWrappedProperty
              ( Function
                  { functionName = "toUpper",
                    functionContents =
                      Property
                        { propertyValue = ObjectValue (NestedObject (UnboundText "otherPerson") (NestedObject (UnboundText "name") ObjectEnd)),
                          propertyAlias = Nothing
                        },
                    functionAlias = Just (UnboundText "upperCaseName")
                  }
              )
          ]

shouldParseWithQuery :: Text -> Clause -> Expectation
shouldParseWithQuery query expectedResult =
  parse parseWith "" query `shouldParse` expectedResult
