module Tokenizer_1_7_1 exposing (..)

import Expect
import GlobalTypes exposing (TokenState(..))
import MockModel exposing (getDefaultModel)
import Test exposing (Test, describe, test)
import Tokenizer


suite : Test
suite =
    let
        testCase =
            "@forename is in (Maksym, Viktor) or @forename is not in (Alex, Julia)"
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
                        , { state = WordTerm
                          , value = "Maksym"
                          , index = 17
                          }
                        , { state = CommaTerm
                          , value = ","
                          , index = 23
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 24
                          }
                        , { state = WordTerm
                          , value = "Viktor"
                          , index = 25
                          }
                        , { state = CloseParenthesisInOperatorTerm
                          , value = ")"
                          , index = 31
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 32
                          }
                        , { state = OrTerm
                          , value = "or"
                          , index = 33
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 35
                          }
                        , { state = KeywordTerm
                          , value = "@forename"
                          , index = 36
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 45
                          }
                        , { state = IsNotInTerm
                          , value = "is not in"
                          , index = 46
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 55
                          }
                        , { state = OpenParenthesisInOperatorTerm
                          , value = "("
                          , index = 56
                          }
                        , { state = WordTerm
                          , value = "Alex"
                          , index = 57
                          }
                        , { state = CommaTerm
                          , value = ","
                          , index = 61
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 62
                          }
                        , { state = WordTerm
                          , value = "Julia"
                          , index = 63
                          }
                        , { state = CloseParenthesisInOperatorTerm
                          , value = ")"
                          , index = 68
                          }
                        ]
            ]
        ]
