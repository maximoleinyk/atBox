module Case_1_2_3 exposing (..)

import Expect
import MockModel exposing (getDefaultModel)
import Test exposing (Test, describe, test)
import Tokenizer exposing (TokenState(..))


suite : Test
suite =
    let
        testCase =
            "find a person whose @name is not Maksym Oliinyk"
    in
    describe "Tokenizer.run"
        [ describe "is not"
            [ test testCase <|
                \_ ->
                    Expect.equal (Tokenizer.run testCase getDefaultModel)
                        [ { state = WordTerm
                          , parsedToken =
                                { string = "find"
                                , length = 4
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
                                { string = "a"
                                , length = 1
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
                                { string = "person"
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
                                { string = "whose"
                                , length = 5
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
                        , { state = NotTerm
                          , parsedToken =
                                { string = "not"
                                , length = 3
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
                        ]
            ]
        ]
