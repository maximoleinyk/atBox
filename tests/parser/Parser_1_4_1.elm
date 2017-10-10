module Parser_1_4_1 exposing (..)

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
            "@forename is Maksym and @surname Oliinyk and @age is 26"
    in
    describe "Parser.run"
        [ describe "and"
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
                            { left =
                                Node
                                    { left =
                                        Node
                                            { left = Leaf "@forename"
                                            , value = "is"
                                            , right = Leaf "Maksym"
                                            }
                                    , value = "and"
                                    , right = Null
                                    }
                            , value = "and"
                            , right =
                                Node
                                    { left = Leaf "@age"
                                    , value = "is"
                                    , right = Leaf "26"
                                    }
                            }
                        )
            ]
        ]
