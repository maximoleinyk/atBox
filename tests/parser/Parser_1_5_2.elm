module Parser_1_5_2 exposing (..)

import Expect
import GlobalTypes exposing (AST(..), OutputOperatorType(AndOperatorType, IsEitherOperatorType, IsNeitherOperatorType, IsOperatorType, OrOperatorType), OutputValueType(MultipleValues, SingleValue))
import Lexer
import MockModel exposing (getDefaultModel)
import Parser
import Test exposing (Test, describe, test)
import Tokenizer


suite : Test
suite =
    let
        testCase =
            "@forename is Maksym and @surname is either Ivanov or Petrov or @forename is Viktor and @surname is neither Sokolov nor Smirnov"
    in
    describe "Parser.run"
        [ describe "or"
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
                                    , right =
                                        Node
                                            { left = Leaf (SingleValue "surname")
                                            , value = IsEitherOperatorType
                                            , right = Leaf (MultipleValues [ "Ivanov", "Petrov" ])
                                            }
                                    }
                            , value = OrOperatorType
                            , right =
                                Node
                                    { left =
                                        Node
                                            { left = Leaf (SingleValue "forename")
                                            , value = IsOperatorType
                                            , right = Leaf (SingleValue "Viktor")
                                            }
                                    , value = AndOperatorType
                                    , right =
                                        Node
                                            { left = Leaf (SingleValue "surname")
                                            , value = IsNeitherOperatorType
                                            , right = Leaf (MultipleValues [ "Sokolov", "Smirnov" ])
                                            }
                                    }
                            }
                        )
            ]
        ]
