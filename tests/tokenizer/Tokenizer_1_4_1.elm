module Tokenizer_1_4_1 exposing (..)

import Expect
import GlobalTypes exposing (TokenState(..))
import MockModel exposing (getDefaultModel)
import Test exposing (Test, describe, test)
import Tokenizer


suite : Test
suite =
    let
        testCase =
            "@forename is Maksym and @surname Oliinyk and @age is 26"
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
                        , { state = AndTerm
                          , value = "and"
                          , index = 20
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 23
                          }
                        , { state = KeywordTerm
                          , value = "@surname"
                          , index = 24
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 32
                          }
                        , { state = WordTerm
                          , value = "Oliinyk"
                          , index = 33
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 40
                          }
                        , { state = AndTerm
                          , value = "and"
                          , index = 41
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 44
                          }
                        , { state = KeywordTerm
                          , value = "@age"
                          , index = 45
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 49
                          }
                        , { state = IsTerm
                          , value = "is"
                          , index = 50
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 52
                          }
                        , { state = WordTerm
                          , value = "26"
                          , index = 53
                          }
                        ]
            ]
        ]
