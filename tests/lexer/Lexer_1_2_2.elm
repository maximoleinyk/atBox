module Lexer_1_2_2 exposing (..)

import Expect
import GlobalTypes exposing (LexemeType(Field, LexemeValue, Operator), OperatorType(IsNotType, IsType))
import Lexer
import MockModel exposing (getDefaultModel)
import Test exposing (Test, describe, test)
import Tokenizer


suite : Test
suite =
    let
        testCase =
            "@name is something not something Maksym"
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
                          , value = "something"
                          , index = 9
                          }
                        ]
            ]
        ]
