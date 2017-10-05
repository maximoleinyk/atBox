module Case_1_7_1 exposing (..)

import Expect
import FsmState exposing (FsmType(..))
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
                    Expect.equal (Tokenizer.run testCase getDefaultModel)
                        [ { state = KeywordTerm
                          , parsedToken =
                                { string = "@forename"
                                , length = 9
                                , remainingString = " is in (Maksym, Viktor) or @forename is not in (Alex, Julia)"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "is in (Maksym, Viktor) or @forename is not in (Alex, Julia)"
                                }
                          }
                        , { state = IsTerm
                          , parsedToken =
                                { string = "is"
                                , length = 2
                                , remainingString = " in (Maksym, Viktor) or @forename is not in (Alex, Julia)"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "in (Maksym, Viktor) or @forename is not in (Alex, Julia)"
                                }
                          }
                        , { state = InTerm
                          , parsedToken =
                                { string = "in"
                                , length = 2
                                , remainingString = " (Maksym, Viktor) or @forename is not in (Alex, Julia)"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "(Maksym, Viktor) or @forename is not in (Alex, Julia)"
                                }
                          }
                        , { state = OpenParenthesisTerm
                          , parsedToken =
                                { string = "("
                                , length = 1
                                , remainingString = "Maksym, Viktor) or @forename is not in (Alex, Julia)"
                                }
                          }
                        , { state = WordTerm
                          , parsedToken =
                                { string = "Maksym"
                                , length = 6
                                , remainingString = ", Viktor) or @forename is not in (Alex, Julia)"
                                }
                          }
                        , { state = CommaTerm
                          , parsedToken =
                                { string = ","
                                , length = 1
                                , remainingString = " Viktor) or @forename is not in (Alex, Julia)"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "Viktor) or @forename is not in (Alex, Julia)"
                                }
                          }
                        , { state = WordTerm
                          , parsedToken =
                                { string = "Viktor"
                                , length = 6
                                , remainingString = ") or @forename is not in (Alex, Julia)"
                                }
                          }
                        , { state = CloseParenthesisTerm
                          , parsedToken =
                                { string = ")"
                                , length = 1
                                , remainingString = " or @forename is not in (Alex, Julia)"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "or @forename is not in (Alex, Julia)"
                                }
                          }
                        , { state = OrTerm
                          , parsedToken =
                                { string = "or"
                                , length = 2
                                , remainingString = " @forename is not in (Alex, Julia)"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "@forename is not in (Alex, Julia)"
                                }
                          }
                        , { state = KeywordTerm
                          , parsedToken =
                                { string = "@forename"
                                , length = 9
                                , remainingString = " is not in (Alex, Julia)"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "is not in (Alex, Julia)"
                                }
                          }
                        , { state = IsTerm
                          , parsedToken =
                                { string = "is"
                                , length = 2
                                , remainingString = " not in (Alex, Julia)"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "not in (Alex, Julia)"
                                }
                          }
                        , { state = NotTerm
                          , parsedToken =
                                { string = "not"
                                , length = 3
                                , remainingString = " in (Alex, Julia)"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "in (Alex, Julia)"
                                }
                          }
                        , { state = InTerm
                          , parsedToken =
                                { string = "in"
                                , length = 2
                                , remainingString = " (Alex, Julia)"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "(Alex, Julia)"
                                }
                          }
                        , { state = OpenParenthesisTerm
                          , parsedToken =
                                { string = "("
                                , length = 1
                                , remainingString = "Alex, Julia)"
                                }
                          }
                        , { state = WordTerm
                          , parsedToken =
                                { string = "Alex"
                                , length = 4
                                , remainingString = ", Julia)"
                                }
                          }
                        , { state = CommaTerm
                          , parsedToken =
                                { string = ","
                                , length = 1
                                , remainingString = " Julia)"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "Julia)"
                                }
                          }
                        , { state = WordTerm
                          , parsedToken =
                                { string = "Julia"
                                , length = 5
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
