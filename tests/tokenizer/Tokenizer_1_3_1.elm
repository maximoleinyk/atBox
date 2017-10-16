module Tokenizer_1_3_1 exposing (..)

import Expect
import GlobalTypes exposing (TokenState(..))
import MockModel exposing (getDefaultModel)
import Test exposing (Test, describe, test)
import Tokenizer


suite : Test
suite =
    let
        testCase =
            "@forename is either Maksym or Viktor"
    in
    describe "Tokenizer.run"
        [ describe "is either"
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
                        , { state = IsEitherTerm
                          , value = "is either"
                          , index = 10
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 19
                          }
                        , { state = WordTerm
                          , value = "Maksym"
                          , index = 20
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 26
                          }
                        , { state = EitherOrTerm
                          , value = "or"
                          , index = 27
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 29
                          }
                        , { state = WordTerm
                          , value = "Viktor"
                          , index = 30
                          }
                        ]
            ]
        ]
