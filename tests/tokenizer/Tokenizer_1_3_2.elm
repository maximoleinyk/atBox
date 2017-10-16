module Tokenizer_1_3_2 exposing (..)

import Expect
import GlobalTypes exposing (TokenState(..))
import MockModel exposing (getDefaultModel)
import Test exposing (Test, describe, test)
import Tokenizer


suite : Test
suite =
    let
        testCase =
            "@forename is neither Maksym nor Viktor"
    in
    describe "Tokenizer.run"
        [ describe "is neither"
            [ test testCase <|
                \_ ->
                    let
                        ( tokens, remainingStates ) =
                            Tokenizer.run testCase getDefaultModel
                    in
                    Expect.equal tokens
                        [ { state = KeywordTerm
                          , value = "@forename"
                          , index = 0
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 9
                          }
                        , { state = IsNeitherTerm
                          , value = "is neither"
                          , index = 10
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 20
                          }
                        , { state = WordTerm
                          , value = "Maksym"
                          , index = 21
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 27
                          }
                        , { state = NeitherNorTerm
                          , value = "nor"
                          , index = 28
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 31
                          }
                        , { state = WordTerm
                          , value = "Viktor"
                          , index = 32
                          }
                        ]
            ]
        ]
