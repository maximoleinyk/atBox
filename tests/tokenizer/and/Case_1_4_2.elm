module Case_1_4_2 exposing (..)

import Expect
import MockModel exposing (getDefaultModel)
import Test exposing (Test, describe, test)
import Tokenizer exposing (TokenState(..))


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
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                }
                          }
                        , { state = IsTerm
                          , parsedToken =
                                { string = "is"
                                , length = 2
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                }
                          }
                        , { state = WordTerm
                          , parsedToken =
                                { string = "26"
                                , length = 2
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                }
                          }
                        , { state = AndTerm
                          , parsedToken =
                                { string = "and"
                                , length = 3
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                }
                          }
                        , { state = KeywordTerm
                          , parsedToken =
                                { string = "@name"
                                , length = 5
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                }
                          }
                        , { state = IsTerm
                          , parsedToken =
                                { string = "is"
                                , length = 2
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                }
                          }
                        , { state = StartQuoteTerm
                          , parsedToken =
                                { string = "\""
                                , length = 1
                                }
                          }
                        , { state = WordTerm
                          , parsedToken =
                                { string = "Maksym"
                                , length = 6
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                }
                          }
                        , { state = WordTerm
                          , parsedToken =
                                { string = "Oliinyk"
                                , length = 7
                                }
                          }
                        , { state = EndQuoteTerm
                          , parsedToken =
                                { string = "\""
                                , length = 1
                                }
                          }
                        ]
            ]
        ]
