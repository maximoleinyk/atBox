module Case_1_1_1 exposing (..)

import Expect
import FsmState exposing (FsmState(..))
import Lexer
import MockModel exposing (getDefaultModel)
import Test exposing (Test, describe, test)


suite : Test
suite =
    let
        testCase =
            "@name is Max"
    in
    describe "Lexer.evaluate"
        [ describe "is"
            [ test testCase <|
                \_ ->
                    Expect.equal (Lexer.evaluate testCase getDefaultModel)
                        [ { state = KeywordTerm
                          , parsedToken =
                                { string = "@name"
                                , length = 5
                                , remainingString = " is Max"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "is Max"
                                }
                          }
                        , { state = IsTerm
                          , parsedToken =
                                { string = "is"
                                , length = 2
                                , remainingString = " Max"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "Max"
                                }
                          }
                        , { state = WordTerm
                          , parsedToken =
                                { string = "Max"
                                , length = 3
                                , remainingString = ""
                                }
                          }
                        ]
            ]
        ]
