module Lexer_1_1_8 exposing (..)

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
            "@@@name is Max"
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
                        []
            ]
        ]