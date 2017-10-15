module Parser_1_1_7 exposing (..)

import Expect
import GlobalTypes exposing (AST(..))
import Lexer
import MockModel exposing (getDefaultModel)
import Parser
import Test exposing (Test, describe, test)
import Tokenizer


suite : Test
suite =
    let
        testCase =
            "find@name is Maksym"
    in
    describe "Parser.run"
        [ describe "is"
            [ test testCase <|
                \_ ->
                    let
                        model =
                            getDefaultModel

                        ( tokens, remainingStates ) =
                            Tokenizer.run testCase model

                        lexemes =
                            Lexer.run tokens model
                    in
                    Expect.equal (Parser.run lexemes model)
                        (Node
                            { left = Leaf "@name"
                            , value = "is"
                            , right = Leaf "Maksym"
                            }
                        )
            ]
        ]
