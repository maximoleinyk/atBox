module Case_1_4_2 exposing (..)

import Expect
import MockModel exposing (getDefaultModel)
import Test exposing (Test, describe, test)
import TokenState exposing (TokenState(..))
import Tokenizer


suite : Test
suite =
    let
        testCase =
            "@age is 26 and @name is \"Maksym Oliinyk\""
    in
    describe "Tokenizer.run"
        [ describe "is"
            [ test testCase <|
                \_ ->
                    Expect.equal (Tokenizer.run testCase getDefaultModel)
                        [ { state = KeywordTerm
                          , parsedToken =
                                { string = "@age"
                                , length = 4
                                , remainingString = " is 26 and @name is \"Maksym Oliinyk\""
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "is 26 and @name is \"Maksym Oliinyk\""
                                }
                          }
                        , { state = IsTerm
                          , parsedToken =
                                { string = "is"
                                , length = 2
                                , remainingString = " 26 and @name is \"Maksym Oliinyk\""
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "26 and @name is \"Maksym Oliinyk\""
                                }
                          }
                        , { state = WordTerm
                          , parsedToken =
                                { string = "26"
                                , length = 2
                                , remainingString = " and @name is \"Maksym Oliinyk\""
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "and @name is \"Maksym Oliinyk\""
                                }
                          }
                        , { state = AndTerm
                          , parsedToken =
                                { string = "and"
                                , length = 3
                                , remainingString = " @name is \"Maksym Oliinyk\""
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "@name is \"Maksym Oliinyk\""
                                }
                          }
                        , { state = KeywordTerm
                          , parsedToken =
                                { string = "@name"
                                , length = 5
                                , remainingString = " is \"Maksym Oliinyk\""
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "is \"Maksym Oliinyk\""
                                }
                          }
                        , { state = IsTerm
                          , parsedToken =
                                { string = "is"
                                , length = 2
                                , remainingString = " \"Maksym Oliinyk\""
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "\"Maksym Oliinyk\""
                                }
                          }
                        , { state = StartQuoteTerm
                          , parsedToken =
                                { string = "\""
                                , length = 1
                                , remainingString = "Maksym Oliinyk\""
                                }
                          }
                        , { state = WordTerm
                          , parsedToken =
                                { string = "Maksym"
                                , length = 6
                                , remainingString = " Oliinyk\""
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "Oliinyk\""
                                }
                          }
                        , { state = WordTerm
                          , parsedToken =
                                { string = "Oliinyk"
                                , length = 7
                                , remainingString = "\""
                                }
                          }
                        , { state = EndQuoteTerm
                          , parsedToken =
                                { string = "\""
                                , length = 1
                                , remainingString = ""
                                }
                          }
                        ]
            ]
        ]
