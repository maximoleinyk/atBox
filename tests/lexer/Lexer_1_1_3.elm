module Lexer_1_1_3 exposing (..)

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
            "find a person whose @name is \"Maksym Oliinyk\""
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
                          , index = 20
                          }
                        , { lexemeType = Operator IsType
                          , value = "is"
                          , index = 26
                          }
                        , { lexemeType = LexemeValue
                          , value = "Maksym Oliinyk"
                          , index = 29
                          }
                        ]
            ]
        ]
