module Parser_1_7_1 exposing (..)

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
            "@forename is in (Maksym, Viktor) or @forename is not in (Alex, Julia)"
    in
    describe "Parser.run"
        [ describe "is in / is not in"
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
                            { left = Node { left = Leaf "@forename", value = "in", right = Leaf "(Maksym, Viktor)" }
                            , value = "or"
                            , right = Node { left = Leaf "@forename", value = "in", right = Leaf "(Alex, Julia)" }
                            }
                        )
            ]
        ]
