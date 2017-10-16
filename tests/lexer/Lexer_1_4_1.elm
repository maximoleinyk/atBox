module Lexer_1_4_1 exposing (..)

import Expect
import GlobalTypes exposing (LexemeState(Field, Joiner, LexemeValue, Operator), OperatorType(IsNotType, IsType))
import Lexer
import MockModel exposing (getDefaultModel)
import Test exposing (Test, describe, test)
import Tokenizer


suite : Test
suite =
    let
        testCase =
            "@forename is Maksym and @surname Oliinyk and @age is 26"
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
                          , value = "@forename"
                          , index = 0
                          }
                        , { state = Operator IsType
                          , value = "is"
                          , index = 10
                          }
                        , { state = LexemeValue
                          , value = "Maksym"
                          , index = 13
                          }
                        , { state = Joiner
                          , value = "and"
                          , index = 20
                          }
                        , { state = Field
                          , value = "@surname"
                          , index = 24
                          }
                        , { state = Joiner
                          , value = "and"
                          , index = 41
                          }
                        , { state = Field
                          , value = "@age"
                          , index = 45
                          }
                        , { state = Operator IsType
                          , value = "is"
                          , index = 50
                          }
                        , { state = LexemeValue
                          , value = "26"
                          , index = 53
                          }
                        ]
            ]
        ]
