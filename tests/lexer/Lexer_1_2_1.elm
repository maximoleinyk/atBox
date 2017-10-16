module Lexer_1_2_1 exposing (..)

import Expect
import GlobalTypes exposing (LexemeState(Field, LexemeValue, Operator), OperatorType(IsNotType, IsType))
import Lexer
import MockModel exposing (getDefaultModel)
import Test exposing (Test, describe, test)
import Tokenizer


suite : Test
suite =
    let
        testCase =
            "@name is not Maksym"
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
                        [ { state = Field
                          , value = "@name"
                          , index = 0
                          }
                        , { state = Operator IsNotType
                          , value = "is not"
                          , index = 6
                          }
                        , { state = LexemeValue
                          , value = "Maksym"
                          , index = 13
                          }
                        ]
            ]
        ]
