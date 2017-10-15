module Case_1_1_8 exposing (..)

import Expect
import GlobalTypes exposing (TokenState(..))
import MockModel exposing (getDefaultModel)
import Test exposing (Test, describe, test)
import Tokenizer


suite : Test
suite =
    let
        testCase =
            "@@@name is Max"
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
                        [ { state = UnknownKeywordTerm
                          , value = "@@@name"
                          , index = 0
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 7
                          }
                        , { state = IsTerm
                          , value = "is"
                          , index = 8
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 10
                          }
                        , { state = WordTerm
                          , value = "Max"
                          , index = 11
                          }
                        ]
            ]
        ]
