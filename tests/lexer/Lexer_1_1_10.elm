module Lexer_1_1_10 exposing (..)

import Expect
import GlobalTypes exposing (LexemeState(Field, Joiner))
import Lexer
import MockModel exposing (getDefaultModel)
import Test exposing (Test, describe, test)
import Tokenizer


suite : Test
suite =
    let
        testCase =
            "@name could be or is Maksym Oliinyk"
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
                          , value = "@name"
                          , index = 0
                          }
                        , { state = Joiner
                          , value = "or"
                          , index = 15
                          }
                        ]
            ]
        ]
