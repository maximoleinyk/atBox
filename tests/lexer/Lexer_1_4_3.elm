module Lexer_1_4_3 exposing (..)

import Expect
import GlobalTypes exposing (LexemeType(Field, Joiner, LexemeValue, Operator), OperatorType(IsEitherType, IsNotType, IsType))
import Lexer
import MockModel exposing (getDefaultModel)
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
                          , value = " Ivanov or Petrov"
                          , index = 42
                          }
                        ]
            ]
        ]
