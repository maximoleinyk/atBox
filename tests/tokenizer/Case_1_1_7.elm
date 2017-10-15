module Case_1_1_7 exposing (..)

import Expect
import GlobalTypes exposing (TokenState(..))
import MockModel exposing (getDefaultModel)
import Test exposing (Test, describe, test)
import Tokenizer


suite : Test
suite =
    let
        testCase =
            "find@name is Maksym"
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
                        , { state = KeywordTerm
                          , value = "@name"
                          , index = 4
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
                        ]
            ]
        ]
