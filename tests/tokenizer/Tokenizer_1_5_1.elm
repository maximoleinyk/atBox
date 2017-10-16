module Tokenizer_1_5_1 exposing (..)

import Expect
import GlobalTypes exposing (TokenState(..))
import MockModel exposing (getDefaultModel)
import Test exposing (Test, describe, test)
import Tokenizer


suite : Test
suite =
    let
        testCase =
            "@forename is Maksym or @surname Oliinyk"
    in
    describe "Tokenizer.run"
        [ describe "or"
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
                        , { state = IsTerm
                          , value = "is"
                          , index = 10
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 12
                          }
                        , { state = WordTerm
                          , value = "Maksym"
                          , index = 13
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 19
                          }
                        , { state = OrTerm
                          , value = "or"
                          , index = 20
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 22
                          }
                        , { state = KeywordTerm
                          , value = "@surname"
                          , index = 23
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 31
                          }
                        , { state = WordTerm
                          , value = "Oliinyk"
                          , index = 32
                          }
                        ]
            ]
        ]
