module Case_1_4_1 exposing (..)

import Expect
import MockModel exposing (getDefaultModel)
import Test exposing (Test, describe, test)
import TokenState exposing (TokenState(..))
import Tokenizer


suite : Test
suite =
    let
        testCase =
            "@forename is Maksym and @surname Oliinyk and @age is 26"
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
                                , remainingString = " is Maksym and @surname Oliinyk and @age is 26"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "is Maksym and @surname Oliinyk and @age is 26"
                                }
                          }
                        , { state = IsTerm
                          , parsedToken =
                                { string = "is"
                                , length = 2
                                , remainingString = " Maksym and @surname Oliinyk and @age is 26"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "Maksym and @surname Oliinyk and @age is 26"
                                }
                          }
                        , { state = WordTerm
                          , parsedToken =
                                { string = "Maksym"
                                , length = 6
                                , remainingString = " and @surname Oliinyk and @age is 26"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "and @surname Oliinyk and @age is 26"
                                }
                          }
                        , { state = AndTerm
                          , parsedToken =
                                { string = "and"
                                , length = 3
                                , remainingString = " @surname Oliinyk and @age is 26"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "@surname Oliinyk and @age is 26"
                                }
                          }
                        , { state = KeywordTerm
                          , parsedToken =
                                { string = "@surname"
                                , length = 8
                                , remainingString = " Oliinyk and @age is 26"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "Oliinyk and @age is 26"
                                }
                          }
                        , { state = WordTerm
                          , parsedToken =
                                { string = "Oliinyk"
                                , length = 7
                                , remainingString = " and @age is 26"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "and @age is 26"
                                }
                          }
                        , { state = AndTerm
                          , parsedToken =
                                { string = "and"
                                , length = 3
                                , remainingString = " @age is 26"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "@age is 26"
                                }
                          }
                        , { state = KeywordTerm
                          , parsedToken =
                                { string = "@age"
                                , length = 4
                                , remainingString = " is 26"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "is 26"
                                }
                          }
                        , { state = IsTerm
                          , parsedToken =
                                { string = "is"
                                , length = 2
                                , remainingString = " 26"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "26"
                                }
                          }
                        , { state = WordTerm
                          , parsedToken =
                                { string = "26"
                                , length = 2
                                , remainingString = ""
                                }
                          }
                        ]
            ]
        ]
