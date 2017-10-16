module Parser_1_4_3 exposing (..)

import Expect
import GlobalTypes exposing (AST(..), OutputOperatorType(AndOperatorType, IsEitherOperatorType, IsOperatorType), OutputValueType(MultipleValues, SingleValue))
import Lexer
import MockModel exposing (getDefaultModel)
import Parser
import Test exposing (Test, describe, test)
import Tokenizer


suite : Test
suite =
    let
        testCase =
            "@forename is Maksym and @surname is either Ivanov or Petrov"
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
                        )
            ]
        ]
