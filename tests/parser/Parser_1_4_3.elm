module Parser_1_4_3 exposing (..)

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
            "@forename is Maksym and @surname is either Ivanov or Petrov"
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
                                    { left = Leaf "@forename"
                                    , value = "is"
                                    , right = Leaf "Maksym"
                                    }
                            , value = "and"
                            , right =
                                Node
                                    { left = Leaf "@surname"
                                    , value = "either"
                                    , right = Leaf " Ivanov or Petrov"
                                    }
                            }
                        )
            ]
        ]
