module Tokenizer_1_1_9 exposing (..)

import Expect
import GlobalTypes exposing (TokenState(..))
import MockModel exposing (getDefaultModel)
import Test exposing (Test, describe, test)
import Tokenizer


suite : Test
suite =
    let
        testCase =
            "@nonexistingfield is Max"
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
                          , value = "@nonexistingfield"
                          , index = 0
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 17
                          }
                        , { state = IsTerm
                          , value = "is"
                          , index = 18
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 20
                          }
                        , { state = WordTerm
                          , value = "Max"
                          , index = 21
                          }
                        ]
            ]
        ]
