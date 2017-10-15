module Lexer_1_5_2 exposing (..)

import Expect
import GlobalTypes exposing (LexemeType(..), OperatorType(..))
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
                        [ { lexemeType = Field
                          , value = "@forename"
                          , index = 0
                          }
                        , { lexemeType = Operator IsType
                          , value = "is"
                          , index = 10
                          }
                        , { lexemeType = LexemeValue
                          , value = "Maksym"
                          , index = 13
                          }
                        , { lexemeType = Joiner
                          , value = "and"
                          , index = 20
                          }
                        , { lexemeType = Field
                          , value = "@surname"
                          , index = 24
                          }
                        , { lexemeType = Operator IsEitherType
                          , value = "is either"
                          , index = 33
                          }
                        , { lexemeType = LexemeValue
                          , value = " Ivanov or Petrov "
                          , index = 42
                          }
                        , { lexemeType = Joiner
                          , value = "or"
                          , index = 60
                          }
                        , { lexemeType = Field
                          , value = "@forename"
                          , index = 63
                          }
                        , { lexemeType = Operator IsType
                          , value = "is"
                          , index = 73
                          }
                        , { lexemeType = LexemeValue
                          , value = "Viktor"
                          , index = 76
                          }
                        , { lexemeType = Joiner
                          , value = "and"
                          , index = 83
                          }
                        , { lexemeType = Field
                          , value = "@surname"
                          , index = 87
                          }
                        , { lexemeType = Operator IsNeitherType
                          , value = "is neither"
                          , index = 96
                          }
                        , { lexemeType = LexemeValue
                          , value = " Sokolov nor Smirnov"
                          , index = 106
                          }
                        ]
            ]
        ]
