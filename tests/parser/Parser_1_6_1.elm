module Parser_1_6_1 exposing (..)

import Expect
import GlobalTypes exposing (AST(..), OutputOperatorType(IsInOperatorType), OutputValueType(MultipleValues, SingleValue))
import Lexer
import MockModel exposing (getDefaultModel)
import Parser
import Test exposing (Test, describe, test)
import Tokenizer


suite : Test
suite =
    let
        testCase =
            "@forename is in (Maksym, Viktor)"
    in
    describe "Parser.run"
        [ describe "in"
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
                            , value = IsInOperatorType
                            , right = Leaf (MultipleValues [ "Maksym", " Viktor" ])
                            }
                        )
            ]
        ]
