module Case_1_2_3 exposing (..)

import Expect
import GlobalTypes exposing (TokenState(..))
import MockModel exposing (getDefaultModel)
import Test exposing (Test, describe, test)
import Tokenizer


suite : Test
suite =
    let
        testCase =
            "find a person whose @name is not Maksym Oliinyk"
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
                        , { state = KeywordTerm
                          , value = "@name"
                          , index = 20
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 25
                          }
                        , { state = IsNotTerm
                          , value = "is not"
                          , index = 26
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 32
                          }
                        , { state = WordTerm
                          , value = "Maksym"
                          , index = 33
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 39
                          }
                        , { state = WordTerm
                          , value = "Oliinyk"
                          , index = 40
                          }
                        ]
            ]
        ]
