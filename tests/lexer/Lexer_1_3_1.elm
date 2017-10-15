module Lexer_1_3_1 exposing (..)

import Expect
import GlobalTypes exposing (LexemeType(Field, LexemeValue, Operator), OperatorType(IsEitherType, IsNotType, IsType))
import Lexer
import MockModel exposing (getDefaultModel)
import Test exposing (Test, describe, test)
import Tokenizer


suite : Test
suite =
    let
        testCase =
            "@forename is either Maksym or Viktor"
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
                        , { lexemeType = Operator IsEitherType
                          , value = "is either"
                          , index = 10
                          }
                        , { lexemeType = LexemeValue
                          , value = " Maksym or Viktor"
                          , index = 19
                          }
                        ]
            ]
        ]
