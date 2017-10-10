module Parser_1_5_1 exposing (..)

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
            "@forename is Maksym or @surname Oliinyk"
    in
    describe "Parser.run"
        [ describe "or"
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
                            , value = "or"
                            , right = Nil
                            }
                        )
            ]
        ]
