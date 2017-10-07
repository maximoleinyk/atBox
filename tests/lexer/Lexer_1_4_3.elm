module Lexer_1_4_3 exposing (..)

import Expect
import Lexer exposing (LexemeType(..), LexerState(..))
import MockModel exposing (getDefaultModel)
import OperatorType exposing (OperatorType(..))
import Test exposing (Test, describe, test)
import Tokenizer


suite : Test
suite =
    let
        testCase =
            "@forename is Maksym and @surname is either Ivanov or Petrov"
    in
    describe "Lexer.run"
        [ describe "is"
            [ test testCase <|
                \_ ->
                    let
                        model =
                            getDefaultModel

                        tokens =
                            Tokenizer.run testCase model
                    in
                    Expect.equal (Lexer.run tokens model)
                        [ { lexemeType = Field, value = "@forename" }
                        , { lexemeType = Operator IsType, value = "is" }
                        , { lexemeType = Value, value = "Maksym" }
                        , { lexemeType = Joiner, value = "and" }
                        , { lexemeType = Field, value = "@surname" }
                        , { lexemeType = Operator IsEitherType, value = "either" }
                        , { lexemeType = Value, value = " Ivanov or Petrov" }
                        ]
            ]
        ]
