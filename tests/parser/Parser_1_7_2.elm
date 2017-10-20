module Parser_1_7_2 exposing (..)

import Expect
import GlobalTypes exposing (AST(..), OutputOperatorType(AndOperatorType, IsEitherOperatorType, IsInOperatorType, IsNeitherOperatorType, IsNotInOperatorType, IsNotOperatorType, IsOperatorType, OrOperatorType), OutputValueType(MultipleValues, SingleValue))
import Lexer
import MockModel exposing (getDefaultModel)
import Parser
import Test exposing (Test, describe, test)
import Tokenizer


suite : Test
suite =
    let
        testCase =
            "((@name is either Max or Joe) or (@surname is neither Oliinyk nor Doe)) and ((@age is not 27) or (@forename is Oliinyk)) or @surname is not Smirnov"
    in
    describe "Parser.run"
        [ describe "is in / is not in"
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
                                            { left =
                                                Node
                                                    { left = Leaf (SingleValue "name")
                                                    , value = IsEitherOperatorType
                                                    , right = Leaf (MultipleValues [ "Max", "Joe" ])
                                                    }
                                            , value = OrOperatorType
                                            , right =
                                                Node
                                                    { left = Leaf (SingleValue "surname")
                                                    , value = IsNeitherOperatorType
                                                    , right = Leaf (MultipleValues [ "Oliinyk", "Doe" ])
                                                    }
                                            }
                                    , value = AndOperatorType
                                    , right =
                                        Node
                                            { left =
                                                Node
                                                    { left = Leaf (SingleValue "age")
                                                    , value = IsNotOperatorType
                                                    , right = Leaf (SingleValue "27")
                                                    }
                                            , value = OrOperatorType
                                            , right =
                                                Node
                                                    { left = Leaf (SingleValue "forename")
                                                    , value = IsOperatorType
                                                    , right = Leaf (SingleValue "Oliinyk")
                                                    }
                                            }
                                    }
                            , value = OrOperatorType
                            , right =
                                Node
                                    { left = Leaf (SingleValue "surname")
                                    , value = IsNotOperatorType
                                    , right = Leaf (SingleValue "Smirnov")
                                    }
                            }
                        )
            ]
        ]
