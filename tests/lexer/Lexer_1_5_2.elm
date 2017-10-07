module Lexer_1_5_2 exposing (..)

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
            "@forename is Maksym and @surname is either Ivanov or Petrov or @forename is Viktor and @surname is neither Sokolov nor Smirnov"
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
                        , { lexemeType = Value, value = " Ivanov or Petrov " }
                        , { lexemeType = Joiner, value = "or" }
                        , { lexemeType = Field, value = "@forename" }
                        , { lexemeType = Operator IsType, value = "is" }
                        , { lexemeType = Value, value = "Viktor" }
                        , { lexemeType = Joiner, value = "and" }
                        , { lexemeType = Field, value = "@surname" }
                        , { lexemeType = Operator IsNeitherType, value = "neither" }
                        , { lexemeType = Value, value = " Sokolov nor Smirnov" }
                        ]
            ]
        ]
