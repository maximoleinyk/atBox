module Lexer_1_6_2 exposing (..)

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
            "@forename is in ()"
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
                        , { state = Operator IsInType
                          , value = "is in"
                          , index = 10
                          }
                        , { state = LexemeValue
                          , value = "()"
                          , index = 15
                          }
                        ]
            ]
        ]
