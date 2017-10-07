module Lexer_1_4_2 exposing (..)

import Expect
import Lexer exposing (LexemeType(..), LexerState(..))
import MockModel exposing (getDefaultModel)
import OperatorType exposing (OperatorType(..))
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

                        tokens =
                            Tokenizer.run testCase model
                    in
                    Expect.equal (Lexer.run tokens model)
                        [ { lexemeType = Field, value = "@age" }
                        , { lexemeType = Operator IsType, value = "is" }
                        , { lexemeType = Value, value = "26" }
                        , { lexemeType = Joiner, value = "and" }
                        , { lexemeType = Field, value = "@name" }
                        , { lexemeType = Operator IsType, value = "is" }
                        , { lexemeType = Value, value = "Maksym Oliinyk" }
                        ]
            ]
        ]
