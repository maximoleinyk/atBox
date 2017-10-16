module Lexer_1_5_2 exposing (..)

import Expect
import GlobalTypes exposing (LexemeState(..), OperatorType(..))
import Lexer
import MockModel exposing (getDefaultModel)
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

                        ( tokens, remainingStates ) =
                            Tokenizer.run testCase model
                    in
                    Expect.equal (Lexer.run tokens model)
                        [ { state = Field
                          , value = "@forename"
                          , index = 0
                          }
                        , { state = Operator IsType
                          , value = "is"
                          , index = 10
                          }
                        , { state = LexemeValue
                          , value = "Maksym"
                          , index = 13
                          }
                        , { state = Joiner
                          , value = "and"
                          , index = 20
                          }
                        , { state = Field
                          , value = "@surname"
                          , index = 24
                          }
                        , { state = Operator IsEitherType
                          , value = "is either"
                          , index = 33
                          }
                        , { state = LexemeValue
                          , value = " Ivanov or Petrov "
                          , index = 42
                          }
                        , { state = Joiner
                          , value = "or"
                          , index = 60
                          }
                        , { state = Field
                          , value = "@forename"
                          , index = 63
                          }
                        , { state = Operator IsType
                          , value = "is"
                          , index = 73
                          }
                        , { state = LexemeValue
                          , value = "Viktor"
                          , index = 76
                          }
                        , { state = Joiner
                          , value = "and"
                          , index = 83
                          }
                        , { state = Field
                          , value = "@surname"
                          , index = 87
                          }
                        , { state = Operator IsNeitherType
                          , value = "is neither"
                          , index = 96
                          }
                        , { state = LexemeValue
                          , value = " Sokolov nor Smirnov"
                          , index = 106
                          }
                        ]
            ]
        ]
