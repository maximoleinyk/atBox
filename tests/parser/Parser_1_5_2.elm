module Parser_1_5_2 exposing (..)

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
                                            { left = Leaf "@forename"
                                            , value = "is"
                                            , right = Leaf "Maksym"
                                            }
                                    , value = "and"
                                    , right =
                                        Node
                                            { left = Leaf "@surname"
                                            , value = "is either"
                                            , right = Leaf " Ivanov or Petrov "
                                            }
                                    }
                            , value = "or"
                            , right =
                                Node
                                    { left =
                                        Node
                                            { left = Leaf "@forename"
                                            , value = "is"
                                            , right = Leaf "Viktor"
                                            }
                                    , value = "and"
                                    , right =
                                        Node
                                            { left = Leaf "@surname"
                                            , value = "is neither"
                                            , right = Leaf " Sokolov nor Smirnov"
                                            }
                                    }
                            }
                        )
            ]
        ]
