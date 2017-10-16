module Tokenizer_1_1_5 exposing (..)

import Expect
import GlobalTypes exposing (TokenState(..))
import MockModel exposing (getDefaultModel)
import Test exposing (Test, describe, test)
import Tokenizer


suite : Test
suite =
    let
        testCase =
            "find a person whose name Maksym"
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
                        [ { state = WordTerm
                          , value = "find"
                          , index = 0
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 4
                          }
                        , { state = WordTerm
                          , value = "a"
                          , index = 5
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 6
                          }
                        , { state = WordTerm
                          , value = "person"
                          , index = 7
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 13
                          }
                        , { state = WordTerm
                          , value = "whose"
                          , index = 14
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 19
                          }
                        , { state = WordTerm
                          , value = "name"
                          , index = 20
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 24
                          }
                        , { state = WordTerm
                          , value = "Maksym"
                          , index = 25
                          }
                        ]
            ]
        ]
