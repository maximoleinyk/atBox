module Tokenizer_1_2_1 exposing (..)

import Expect
import GlobalTypes exposing (TokenState(..))
import MockModel exposing (getDefaultModel)
import Test exposing (Test, describe, test)
import Tokenizer


suite : Test
suite =
    let
        testCase =
            "@name is not Maksym"
    in
    describe "Tokenizer.run"
        [ describe "is not"
            [ test testCase <|
                \_ ->
                    let
                        ( tokens, remainingStates ) =
                            Tokenizer.run testCase getDefaultModel
                    in
                    Expect.equal tokens
                        [ { state = KeywordTerm
                          , value = "@name"
                          , index = 0
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 5
                          }
                        , { state = IsNotTerm
                          , value = "is not"
                          , index = 6
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 12
                          }
                        , { state = WordTerm
                          , value = "Maksym"
                          , index = 13
                          }
                        ]
            ]
        ]
