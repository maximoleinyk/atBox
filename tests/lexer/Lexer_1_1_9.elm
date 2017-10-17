module Lexer_1_1_9 exposing (..)

import Expect
import GlobalTypes exposing (LexemeState(LexemeValue, Operator, UnknownField), OperatorType(IsType))
import Lexer
import MockModel exposing (getDefaultModel)
import Test exposing (Test, describe, test)
import Tokenizer


suite : Test
suite =
    let
        testCase =
            "@nonexistingfield is Max"
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
                        [ { state = UnknownField
                          , value = "@nonexistingfield"
                          , index = 0
                          }
                        , { state = Operator IsType
                          , value = "is"
                          , index = 18
                          }
                        , { state = LexemeValue
                          , value = "Max"
                          , index = 21
                          }
                        ]
            ]
        ]
