module Lexer_1_6_1 exposing (..)

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
            "@forename is in (Maksym, Viktor)"
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
                        ]
            ]
        ]
