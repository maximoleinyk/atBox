module Case_1_2_2 exposing (..)

import Expect
import GlobalTypes exposing (TokenState(..))
import MockModel exposing (getDefaultModel)
import Test exposing (Test, describe, test)
import Tokenizer


suite : Test
suite =
    let
        testCase =
            "@name is something not something Maksym"
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
                        , { state = IsTerm
                          , value = "is"
                          , index = 6
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 8
                          }
                        , { state = WordTerm
                          , value = "something"
                          , index = 9
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 18
                          }
                        , { state = WordTerm
                          , value = "not"
                          , index = 19
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 22
                          }
                        , { state = WordTerm
                          , value = "something"
                          , index = 23
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 32
                          }
                        , { state = WordTerm
                          , value = "Maksym"
                          , index = 33
                          }
                        ]
            ]
        ]
