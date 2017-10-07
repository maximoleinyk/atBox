module Lexer_1_7_1 exposing (..)

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
            "@forename is in (Maksym, Viktor) or @forename is not in (Alex, Julia)"
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
                        , { lexemeType = Operator IsInType, value = "in" }
                        , { lexemeType = Value, value = "(Maksym, Viktor)" }
                        , { lexemeType = Joiner, value = "or" }
                        , { lexemeType = Field, value = "@forename" }
                        , { lexemeType = Operator IsNotInType, value = "in" }
                        , { lexemeType = Value, value = "(Alex, Julia)" }
                        ]
            ]
        ]
