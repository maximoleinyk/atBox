module Case_1_1_8 exposing (..)

import Expect
import FsmState exposing (FsmType(..))
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
                    Expect.equal (Tokenizer.run testCase getDefaultModel)
                        [ { state = UnknownKeywordTerm
                          , parsedToken =
                                { string = "@@@name"
                                , length = 7
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
