module Parser_1_4_1 exposing (..)

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
            "@forename is Maksym and @surname Oliinyk and @age is 26"
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
                                    { left =
                                        Node
                                            { left = Leaf (SingleValue "forename")
                                            , value = IsOperatorType
                                            , right = Leaf (SingleValue "Maksym")
                                            }
                                    , value = AndOperatorType
                                    , right = Null
                                    }
                            , value = AndOperatorType
                            , right =
                                Node
                                    { left = Leaf (SingleValue "age")
                                    , value = IsOperatorType
                                    , right = Leaf (SingleValue "26")
                                    }
                            }
                        )
            ]
        ]
