module Lexer_1_1_4 exposing (..)

import Expect
import GlobalTypes exposing (LexemeType(Field, LexemeValue, Operator), OperatorType(IsType))
import Lexer
import MockModel exposing (getDefaultModel)
import Test exposing (Test, describe, test)
import Tokenizer


suite : Test
suite =
    let
        testCase =
            "@name is \"Maksym Oliinyk\""
    in
    describe "Lexer.run"
        [ describe "is"
            [ test testCase <|
                \_ ->
                    let
                        model =
                            getDefaultModel

                        ( tokens, remainingStates ) =
                            Tokenizer.run testCase model
                    in
                    Expect.equal (Lexer.run tokens model)
                        [ { lexemeType = Field
                          , value = "@name"
                          , index = 0
                          }
                        , { lexemeType = Operator IsType
                          , value = "is"
                          , index = 6
                          }
                        , { lexemeType = LexemeValue
                          , value = "Maksym Oliinyk"
                          , index = 9
                          }
                        ]
            ]
        ]
