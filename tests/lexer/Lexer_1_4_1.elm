module Lexer_1_4_1 exposing (..)

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
            "@forename is Maksym and @surname Oliinyk and @age is 26"
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
                        [ { lexemeType = Field, value = "@forename" }
                        , { lexemeType = Operator IsType, value = "is" }
                        , { lexemeType = Value, value = "Maksym" }
                        , { lexemeType = Joiner, value = "and" }
                        , { lexemeType = Field, value = "@surname" }
                        , { lexemeType = Joiner, value = "and" }
                        , { lexemeType = Field, value = "@age" }
                        , { lexemeType = Operator IsType, value = "is" }
                        , { lexemeType = Value, value = "26" }
                        ]
            ]
        ]
