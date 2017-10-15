module Parser_1_4_2 exposing (..)

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
            "@age is 26 and @name is \"Maksym Oliinyk\""
    in
    describe "Parser.run"
        [ describe "and"
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
                            { left =
                                Node
                                    { left = Leaf "@age"
                                    , value = "is"
                                    , right = Leaf "26"
                                    }
                            , value = "and"
                            , right =
                                Node
                                    { left = Leaf "@name"
                                    , value = "is"
                                    , right = Leaf "Maksym Oliinyk"
                                    }
                            }
                        )
            ]
        ]
