module Tokenizer_1_4_2 exposing (..)

import Expect
import GlobalTypes exposing (TokenState(..))
import MockModel exposing (getDefaultModel)
import Test exposing (Test, describe, test)
import Tokenizer


suite : Test
suite =
    let
        testCase =
            "@age is 26 and @name is \"Maksym Oliinyk\""
    in
    describe "Tokenizer.run"
        [ describe "and"
            [ test testCase <|
                \_ ->
                    let
                        ( tokens, remainingStates ) =
                            Tokenizer.run testCase getDefaultModel
                    in
                    Expect.equal tokens
                        [ { state = KeywordTerm
                          , value = "@age"
                          , index = 0
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 4
                          }
                        , { state = IsTerm
                          , value = "is"
                          , index = 5
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 7
                          }
                        , { state = WordTerm
                          , value = "26"
                          , index = 8
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 10
                          }
                        , { state = AndTerm
                          , value = "and"
                          , index = 11
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 14
                          }
                        , { state = KeywordTerm
                          , value = "@name"
                          , index = 15
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 20
                          }
                        , { state = IsTerm
                          , value = "is"
                          , index = 21
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 23
                          }
                        , { state = StartQuoteTerm
                          , value = "\""
                          , index = 24
                          }
                        , { state = WordTerm
                          , value = "Maksym"
                          , index = 25
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
