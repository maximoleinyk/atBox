module Parser_1_3_1 exposing (..)

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
            "@forename is either Maksym or Viktor"
    in
    describe "Parser.run"
        [ describe "is either"
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
                            { left = Leaf "@forename"
                            , value = "is either"
                            , right = Leaf " Maksym or Viktor"
                            }
                        )
            ]
        ]
