module Lexer_1_4_2 exposing (..)

import Expect
import GlobalTypes exposing (LexemeType(Field, Joiner, LexemeValue, Operator), OperatorType(IsNotType, IsType))
import Lexer
import MockModel exposing (getDefaultModel)
import Test exposing (Test, describe, test)
import Tokenizer


suite : Test
suite =
    let
        testCase =
            "@age is 26 and @name is \"Maksym Oliinyk\""
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
                          , value = "@age"
                          , index = 0
                          }
                        , { lexemeType = Operator IsType
                          , value = "is"
                          , index = 5
                          }
                        , { lexemeType = LexemeValue
                          , value = "26"
                          , index = 8
                          }
                        , { lexemeType = Joiner
                          , value = "and"
                          , index = 11
                          }
                        , { lexemeType = Field
                          , value = "@name"
                          , index = 15
                          }
                        , { lexemeType = Operator IsType
                          , value = "is"
                          , index = 21
                          }
                        , { lexemeType = LexemeValue
                          , value = "Maksym Oliinyk"
                          , index = 24
                          }
                        ]
            ]
        ]
