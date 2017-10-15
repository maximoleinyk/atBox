module Lexer_1_5_1 exposing (..)

import Expect
import GlobalTypes exposing (LexemeType(Field, Joiner, LexemeValue, Operator), OperatorType(IsEitherType, IsNotType, IsType))
import Lexer
import MockModel exposing (getDefaultModel)
import Test exposing (Test, describe, test)
import Tokenizer


suite : Test
suite =
    let
        testCase =
            "@forename is Maksym or @surname Oliinyk"
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
                        [ { lexemeType = Field, value = "@forename", index = 0 }
                        , { lexemeType = Operator IsType, value = "is", index = 10 }
                        , { lexemeType = LexemeValue, value = "Maksym", index = 13 }
                        , { lexemeType = Joiner, value = "or", index = 20 }
                        , { lexemeType = Field, value = "@surname", index = 23 }
                        ]
            ]
        ]
