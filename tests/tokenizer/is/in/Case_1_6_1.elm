module Case_1_6_1 exposing (..)

import Expect
import MockModel exposing (getDefaultModel)
import Test exposing (Test, describe, test)
import TokenState exposing (TokenState(..))
import Tokenizer


suite : Test
suite =
    let
        testCase =
            "@forename is in (Maksym, Viktor)"
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
                                , remainingString = " is in (Maksym, Viktor)"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "is in (Maksym, Viktor)"
                                }
                          }
                        , { state = IsTerm
                          , parsedToken =
                                { string = "is"
                                , length = 2
                                , remainingString = " in (Maksym, Viktor)"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "in (Maksym, Viktor)"
                                }
                          }
                        , { state = InTerm
                          , parsedToken =
                                { string = "in"
                                , length = 2
                                , remainingString = " (Maksym, Viktor)"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "(Maksym, Viktor)"
                                }
                          }
                        , { state = OpenParenthesisTerm
                          , parsedToken =
                                { string = "("
                                , length = 1
                                , remainingString = "Maksym, Viktor)"
                                }
                          }
                        , { state = WordTerm
                          , parsedToken =
                                { string = "Maksym"
                                , length = 6
                                , remainingString = ", Viktor)"
                                }
                          }
                        , { state = CommaTerm
                          , parsedToken =
                                { string = ","
                                , length = 1
                                , remainingString = " Viktor)"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "Viktor)"
                                }
                          }
                        , { state = WordTerm
                          , parsedToken =
                                { string = "Viktor"
                                , length = 6
                                , remainingString = ")"
                                }
                          }
                        , { state = CloseParenthesisTerm
                          , parsedToken =
                                { string = ")"
                                , length = 1
                                , remainingString = ""
                                }
                          }
                        ]
            ]
        ]
