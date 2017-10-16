module Tokenizer_1_6_2 exposing (..)

import Expect
import GlobalTypes exposing (TokenState(..))
import MockModel exposing (getDefaultModel)
import Test exposing (Test, describe, test)
import Tokenizer


suite : Test
suite =
    let
        testCase =
            "@forename is in ()"
    in
    describe "Tokenizer.run"
        [ describe "in"
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
                        , { state = IsInTerm
                          , value = "is in"
                          , index = 10
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 15
                          }
                        , { state = OpenParenthesisInOperatorTerm
                          , value = "("
                          , index = 16
                          }
                        , { state = CloseParenthesisInOperatorTerm
                          , value = ")"
                          , index = 17
                          }
                        ]
            ]
        ]
