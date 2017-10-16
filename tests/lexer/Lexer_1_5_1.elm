module Lexer_1_5_1 exposing (..)

import Expect
import GlobalTypes exposing (LexemeState(Field, Joiner, LexemeValue, Operator), OperatorType(IsEitherType, IsNotType, IsType))
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
                        [ { state = Field, value = "@forename", index = 0 }
                        , { state = Operator IsType, value = "is", index = 10 }
                        , { state = LexemeValue, value = "Maksym", index = 13 }
                        , { state = Joiner, value = "or", index = 20 }
                        , { state = Field, value = "@surname", index = 23 }
                        ]
            ]
        ]
