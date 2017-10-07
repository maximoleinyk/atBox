module Case_1_3_2 exposing (..)

import Expect
import MockModel exposing (getDefaultModel)
import Test exposing (Test, describe, test)
import TokenState exposing (TokenState(..))
import Tokenizer


suite : Test
suite =
    let
        testCase =
            "@forename is either Maksym or Viktor or Julia"
    in
    describe "Tokenizer.run"
        [ describe "is"
            [ test testCase <|
                \_ ->
                    Expect.equal (Tokenizer.run testCase getDefaultModel)
                        [ { state = KeywordTerm
                          , parsedToken =
                                { string = "@forename"
                                , length = 9
                                , remainingString = " is either Maksym or Viktor or Julia"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "is either Maksym or Viktor or Julia"
                                }
                          }
                        , { state = IsTerm
                          , parsedToken =
                                { string = "is"
                                , length = 2
                                , remainingString = " either Maksym or Viktor or Julia"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "either Maksym or Viktor or Julia"
                                }
                          }
                        , { state = EitherTerm
                          , parsedToken =
                                { string = "either"
                                , length = 6
                                , remainingString = " Maksym or Viktor or Julia"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "Maksym or Viktor or Julia"
                                }
                          }
                        , { state = WordTerm
                          , parsedToken =
                                { string = "Maksym"
                                , length = 6
                                , remainingString = " or Viktor or Julia"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "or Viktor or Julia"
                                }
                          }
                        , { state = OrTerm
                          , parsedToken =
                                { string = "or"
                                , length = 2
                                , remainingString = " Viktor or Julia"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "Viktor or Julia"
                                }
                          }
                        , { state = WordTerm
                          , parsedToken =
                                { string = "Viktor"
                                , length = 6
                                , remainingString = " or Julia"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "or Julia"
                                }
                          }
                        , { state = OrTerm
                          , parsedToken =
                                { string = "or"
                                , length = 2
                                , remainingString = " Julia"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "Julia"
                                }
                          }
                        , { state = WordTerm
                          , parsedToken =
                                { string = "Julia"
                                , length = 5
                                , remainingString = ""
                                }
                          }
                        ]
            ]
        ]
