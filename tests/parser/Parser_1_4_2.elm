module Parser_1_4_2 exposing (..)

import Expect
import GlobalTypes exposing (AST(..), OutputOperatorType(AndOperatorType, IsOperatorType), OutputValueType(SingleValue))
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
                                    { left = Leaf (SingleValue "age")
                                    , value = IsOperatorType
                                    , right = Leaf (SingleValue "26")
                                    }
                            , value = AndOperatorType
                            , right =
                                Node
                                    { left = Leaf (SingleValue "name")
                                    , value = IsOperatorType
                                    , right = Leaf (SingleValue "Maksym Oliinyk")
                                    }
                            }
                        )
            ]
        ]
