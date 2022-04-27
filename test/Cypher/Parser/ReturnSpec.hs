module Cypher.Parser.ReturnSpec (runParserReturnTests) where

import Cypher.Parser.Return
import Cypher.Types
import Data.Map qualified as M
import Data.Text as T
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

runParserReturnTests :: SpecWith ()
runParserReturnTests =
  describe "Cypher.Parser.Return" $
    context "when parsing return query" $
      context "with standard clause" runStandardParserReturnTests

runStandardParserReturnTests :: Spec
runStandardParserReturnTests = do
  it "parses return clause with all elements" $
    "RETURN *" `shouldParseReturnQuery` Return False ReturnAllElements
  it "parses return clause with single property" $
    "RETURN n"
      `shouldParseReturnQuery` Return
        False
        ( ReturnExpressions
            [ReturnProperty (Property (TextValue (UnboundText "n")) Nothing)]
        )
  it "parses return clause with multiple properties" $
    "RETURN a AS Alias1, b AS Alias2, c"
      `shouldParseReturnQuery` Return
        False
        ( ReturnExpressions
            [ ReturnProperty
                ( Property
                    (TextValue (UnboundText "a"))
                    (Just (UnboundText "Alias1"))
                ),
              ReturnProperty
                ( Property
                    (TextValue (UnboundText "b"))
                    (Just (UnboundText "Alias2"))
                ),
              ReturnProperty (Property (TextValue (UnboundText "c")) Nothing)
            ]
        )
  it "parses return clause with nested properties" $
    "RETURN a.b.c.d"
      `shouldParseReturnQuery` Return
        False
        ( ReturnExpressions
            [ ReturnProperty
                ( Property
                    ( ObjectValue
                        ( NestedObject
                            (UnboundText "a")
                            ( NestedObject
                                (UnboundText "b")
                                ( NestedObject
                                    (UnboundText "c")
                                    (NestedObject (UnboundText "d") ObjectEnd)
                                )
                            )
                        )
                    )
                    Nothing
                )
            ]
        )
  it "parses return clause with multiple nested properties" $
    "RETURN a.b.c.d, e.f.g.h"
      `shouldParseReturnQuery` Return
        False
        ( ReturnExpressions
            [ ReturnProperty
                ( Property
                    ( ObjectValue
                        ( NestedObject
                            (UnboundText "a")
                            ( NestedObject
                                (UnboundText "b")
                                ( NestedObject
                                    (UnboundText "c")
                                    (NestedObject (UnboundText "d") ObjectEnd)
                                )
                            )
                        )
                    )
                    Nothing
                ),
              ReturnProperty
                ( Property
                    ( ObjectValue
                        ( NestedObject
                            (UnboundText "e")
                            ( NestedObject
                                (UnboundText "f")
                                ( NestedObject
                                    (UnboundText "g")
                                    (NestedObject (UnboundText "h") ObjectEnd)
                                )
                            )
                        )
                    )
                    Nothing
                )
            ]
        )
  it "parses return clause with nested escaped properties" $
    "RETURN `some object`.`@ n$st$d 0bject!`.unescapedText"
      `shouldParseReturnQuery` Return
        False
        ( ReturnExpressions
            [ ReturnProperty
                ( Property
                    ( ObjectValue
                        ( NestedObject
                            (BacktickedText "some object")
                            ( NestedObject
                                (BacktickedText "@ n$st$d 0bject!")
                                (NestedObject (UnboundText "unescapedText") ObjectEnd)
                            )
                        )
                    )
                    Nothing
                )
            ]
        )
  it "parses return clause with aliased property" $
    "RETURN n AS NumberOfEggs"
      `shouldParseReturnQuery` Return
        False
        ( ReturnExpressions
            [ ReturnProperty
                ( Property
                    (TextValue (UnboundText "n"))
                    (Just (UnboundText "NumberOfEggs"))
                )
            ]
        )
  it "parses return clause with aliased literal property" $
    "RETURN n AS `Number Of Eggs`"
      `shouldParseReturnQuery` Return
        False
        ( ReturnExpressions
            [ ReturnProperty
                ( Property
                    (TextValue (UnboundText "n"))
                    (Just (BacktickedText "Number Of Eggs"))
                )
            ]
        )
  it
    "parses return clause with multiple nested properties and aliases and odd casing"
    $ "ReTuRn a.b.`c$$`.d aS ABCD, e.`f.g`.h As EFGH"
      `shouldParseReturnQuery` Return
        False
        ( ReturnExpressions
            [ ReturnProperty
                ( Property
                    ( ObjectValue
                        ( NestedObject
                            (UnboundText "a")
                            ( NestedObject
                                (UnboundText "b")
                                ( NestedObject
                                    (BacktickedText "c$$")
                                    (NestedObject (UnboundText "d") ObjectEnd)
                                )
                            )
                        )
                    )
                    (Just (UnboundText "ABCD"))
                ),
              ReturnProperty
                ( Property
                    ( ObjectValue
                        ( NestedObject
                            (UnboundText "e")
                            ( NestedObject
                                (BacktickedText "f.g")
                                (NestedObject (UnboundText "h") ObjectEnd)
                            )
                        )
                    )
                    (Just (UnboundText "EFGH"))
                )
            ]
        )
  it "parses return clause with literal item in double quotes" $
    "RETURN \"I'm a literal\""
      `shouldParseReturnQuery` Return
        False
        ( ReturnExpressions
            [ ReturnProperty
                (Property (TextValue (QuotedText "I'm a literal")) Nothing)
            ]
        )
  it "parses return clause with pattern" $
    "RETURN (a)-->()"
      `shouldParseReturnQuery` Return
        False
        ( ReturnExpressions
            [ ReturnPattern
                [ Node (AnyPatternComponentType (UnboundText "a")) M.empty,
                  ConnectorDirection AnonymousRightDirection,
                  Node EmptyPatternComponentType M.empty
                ]
            ]
        )
  it "parses return clause with function wrapped pattern" $
    "RETURN count((a)-->())"
      `shouldParseReturnQuery` Return
        False
        ( ReturnExpressions
            [ ReturnFunctionWrappedPattern
                ( Function
                    "count"
                    [ Node (AnyPatternComponentType (UnboundText "a")) M.empty,
                      ConnectorDirection AnonymousRightDirection,
                      Node EmptyPatternComponentType M.empty
                    ]
                )
            ]
        )
  it "parses return clause with function wrapped double" $
    "RETURN float(0.9)"
      `shouldParseReturnQuery` Return
        False
        ( ReturnExpressions
            [ ReturnFunctionWrappedPropertyWithArity
                (Function "float" [DoubleValue 0.9])
            ]
        )
  it "parses return clause with function wrapped integer" $
    "RETURN toFloat(3)"
      `shouldParseReturnQuery` Return
        False
        ( ReturnExpressions
            [ ReturnFunctionWrappedPropertyWithArity
                (Function "toFloat" [IntegerValue 3])
            ]
        )
  it "parses return clause with function wrapped text" $
    "RETURN rTrim(BlahBlahBlah)"
      `shouldParseReturnQuery` Return
        False
        ( ReturnExpressions
            [ ReturnFunctionWrappedPropertyWithArity
                (Function "rTrim" [TextValue (UnboundText "BlahBlahBlah")])
            ]
        )
  it "parses return clause with multi-arity function text" $
    "RETURN complexFunction(1,TRUE,3.4, Hi)"
      `shouldParseReturnQuery` Return
        False
        ( ReturnExpressions
            [ ReturnFunctionWrappedPropertyWithArity
                ( Function
                    "complexFunction"
                    [ IntegerValue 1,
                      BooleanValue True,
                      DoubleValue 3.4,
                      TextValue (UnboundText "Hi")
                    ]
                )
            ]
        )
  it "parses return clause with function wrapped object" $
    "RETURN toInteger(person.age)"
      `shouldParseReturnQuery` Return
        False
        ( ReturnExpressions
            [ ReturnFunctionWrappedPropertyWithArity
                ( Function
                    "toInteger"
                    [ ObjectValue
                        ( NestedObject
                            (UnboundText "person")
                            (NestedObject (UnboundText "age") ObjectEnd)
                        )
                    ]
                )
            ]
        )
  it "parses return clause capturing DISTINCT keyword as first parameter" $
    "RETURN DISTINCT 'something', (a)-->()"
      `shouldParseReturnQuery` Return
        True
        ( ReturnExpressions
            [ ReturnProperty
                (Property (TextValue (QuotedText "something")) Nothing),
              ReturnPattern
                [ Node (AnyPatternComponentType (UnboundText "a")) M.empty,
                  ConnectorDirection AnonymousRightDirection,
                  Node EmptyPatternComponentType M.empty
                ]
            ]
        )

shouldParseReturnQuery :: Text -> Clause -> Expectation
shouldParseReturnQuery query expectedResult =
  parse parseReturn "" query `shouldParse` expectedResult
