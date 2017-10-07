module Case_1_3_4 exposing (..)

import Expect
import MockModel exposing (getDefaultModel)
import Test exposing (Test, describe, test)
import TokenState exposing (TokenState(..))
import Tokenizer


suite : Test
suite =
    let
        testCase =
            "@forename is neither Maksym nor Viktor nor Julia"
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
                                , remainingString = " is neither Maksym nor Viktor nor Julia"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "is neither Maksym nor Viktor nor Julia"
                                }
                          }
                        , { state = IsTerm
                          , parsedToken =
                                { string = "is"
                                , length = 2
                                , remainingString = " neither Maksym nor Viktor nor Julia"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "neither Maksym nor Viktor nor Julia"
                                }
                          }
                        , { state = NeitherTerm
                          , parsedToken =
                                { string = "neither"
                                , length = 7
                                , remainingString = " Maksym nor Viktor nor Julia"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "Maksym nor Viktor nor Julia"
                                }
                          }
                        , { state = WordTerm
                          , parsedToken =
                                { string = "Maksym"
                                , length = 6
                                , remainingString = " nor Viktor nor Julia"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "nor Viktor nor Julia"
                                }
                          }
                        , { state = NorTerm
                          , parsedToken =
                                { string = "nor"
                                , length = 3
                                , remainingString = " Viktor nor Julia"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "Viktor nor Julia"
                                }
                          }
                        , { state = WordTerm
                          , parsedToken =
                                { string = "Viktor"
                                , length = 6
                                , remainingString = " nor Julia"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "nor Julia"
                                }
                          }
                        , { state = WordTerm
                          , parsedToken =
                                { string = "nor"
                                , length = 3
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
