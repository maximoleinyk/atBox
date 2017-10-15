module Lexer_1_7_1 exposing (..)

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
            "@forename is in (Maksym, Viktor) or @forename is not in (Alex, Julia)"
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
                        , { lexemeType = Operator IsInType
                          , value = "is in"
                          , index = 10
                          }
                        , { lexemeType = LexemeValue
                          , value = "(Maksym, Viktor)"
                          , index = 15
                          }
                        , { lexemeType = Joiner
                          , value = "or"
                          , index = 33
                          }
                        , { lexemeType = Field
                          , value = "@forename"
                          , index = 36
                          }
                        , { lexemeType = Operator IsNotInType
                          , value = "is not in"
                          , index = 46
                          }
                        , { lexemeType = LexemeValue
                          , value = "(Alex, Julia)"
                          , index = 55
                          }
                        ]
            ]
        ]
