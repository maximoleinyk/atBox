module Lexer_1_1_2 exposing (..)

import Expect
import GlobalTypes exposing (LexemeState(Field, LexemeValue, Operator), OperatorType(IsType))
import Lexer
import MockModel exposing (getDefaultModel)
import Test exposing (Test, describe, test)
import Tokenizer


suite : Test
suite =
    let
        testCase =
            "find a person whose @name is \"Maksym\""
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
                          , index = 20
                          }
                        , { state = Operator IsType
                          , value = "is"
                          , index = 26
                          }
                        , { state = LexemeValue
                          , value = "Maksym"
                          , index = 29
                          }
                        ]
            ]
        ]
