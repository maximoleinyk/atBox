module Parser_1_2_2 exposing (..)

import Expect
import Lexer exposing (LexemeType(..), LexerState(..))
import MockModel exposing (getDefaultModel)
import OperatorType exposing (OperatorType(..))
import Parser exposing (AST(..))
import Test exposing (Test, describe, test)
import Tokenizer


suite : Test
suite =
    let
        testCase =
            "@name is something not something Maksym"
    in
    describe "Parser.run"
        [ describe "is not"
            [ test testCase <|
                \_ ->
                    let
                        model =
                            getDefaultModel

                        tokens =
                            Tokenizer.run testCase model

                        lexemes =
                            Lexer.run tokens model
                    in
                    Expect.equal (Parser.run lexemes model)
                        (Node
                            { left = Leaf "@name"
                            , value = "is"
                            , right = Leaf "something"
                            }
                        )
            ]
        ]
