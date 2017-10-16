module Parser_1_3_2 exposing (..)

import Expect
import GlobalTypes exposing (AST(..), OutputOperatorType(IsNeitherOperatorType), OutputValueType(MultipleValues, SingleValue))
import Lexer
import MockModel exposing (getDefaultModel)
import Parser
import Test exposing (Test, describe, test)
import Tokenizer


suite : Test
suite =
    let
        testCase =
            "@forename is neither Maksym nor Viktor"
    in
    describe "Parser.run"
        [ describe "is neither"
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
                            { left = Leaf (SingleValue "forename")
                            , value = IsNeitherOperatorType
                            , right = Leaf (MultipleValues [ "Maksym", "Viktor" ])
                            }
                        )
            ]
        ]
