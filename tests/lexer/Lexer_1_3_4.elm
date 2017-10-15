module Lexer_1_3_4 exposing (..)

import Expect
import GlobalTypes exposing (LexemeType(Field, LexemeValue, Operator), OperatorType(IsNeitherType, IsNotType, IsType))
import Lexer
import MockModel exposing (getDefaultModel)
import Test exposing (Test, describe, test)
import Tokenizer


suite : Test
suite =
    let
        testCase =
            "@forename is neither Maksym nor Viktor nor Julia"
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
                          , value = "@forename"
                          , index = 0
                          }
                        , { lexemeType = Operator IsNeitherType
                          , value = "is neither"
                          , index = 10
                          }
                        , { lexemeType = LexemeValue
                          , value = " Maksym nor Viktor nor Julia"
                          , index = 20
                          }
                        ]
            ]
        ]
