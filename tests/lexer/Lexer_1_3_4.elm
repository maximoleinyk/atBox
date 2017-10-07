module Lexer_1_3_4 exposing (..)

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
            "@forename is neither Maksym nor Viktor nor Julia"
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
                        , { lexemeType = Operator IsNeitherType, value = "neither" }
                        , { lexemeType = Value, value = " Maksym nor Viktor nor Julia" }
                        ]
            ]
        ]
