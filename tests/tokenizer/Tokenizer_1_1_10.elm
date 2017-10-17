module Tokenizer_1_1_10 exposing (..)

import Expect
import GlobalTypes exposing (TokenState(..))
import MockModel exposing (getDefaultModel)
import Test exposing (Test, describe, test)
import Tokenizer


suite : Test
suite =
    let
        testCase =
            "@name could be or is Maksym Oliinyk"
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
                        , { state = WordTerm
                          , value = "could"
                          , index = 6
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 11
                          }
                        , { state = WordTerm
                          , value = "be"
                          , index = 12
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 14
                          }
                        , { state = WordTerm
                          , value = "or"
                          , index = 15
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 17
                          }
                        , { state = WordTerm
                          , value = "is"
                          , index = 18
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
                        , { state = WordTerm
                          , value = "Oliinyk"
                          , index = 28
                          }
                        ]
            ]
        ]
