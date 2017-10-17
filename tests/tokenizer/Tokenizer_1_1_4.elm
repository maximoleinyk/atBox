module Tokenizer_1_1_4 exposing (..)

import Expect
import GlobalTypes exposing (TokenState(..))
import MockModel exposing (getDefaultModel)
import Test exposing (Test, describe, test)
import Tokenizer


suite : Test
suite =
    let
        testCase =
            "@name is \"Maksym Oliinyk\""
    in
    describe "Tokenizer.run"
        [ describe "is"
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
                        , { state = IsTerm
                          , value = "is"
                          , index = 6
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 8
                          }
                        , { state = StartQuoteTerm
                          , value = "\""
                          , index = 9
                          }
                        , { state = WordTerm
                          , value = "Maksym"
                          , index = 10
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 16
                          }
                        , { state = WordTerm
                          , value = "Oliinyk"
                          , index = 17
                          }
                        , { state = EndQuoteTerm
                          , value = "\""
                          , index = 24
                          }
                        ]
            ]
        ]
