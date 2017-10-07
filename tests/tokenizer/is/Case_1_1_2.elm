module Case_1_1_2 exposing (..)

import Expect
import MockModel exposing (getDefaultModel)
import Test exposing (Test, describe, test)
import TokenState exposing (TokenState(..))
import Tokenizer


suite : Test
suite =
    let
        testCase =
            "find a person whose @name is \"Maksym\""
    in
    describe "Tokenizer.run"
        [ describe "is"
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
                        , { state = EndQuoteTerm
                          , parsedToken =
                                { string = "\""
                                , length = 1
                                }
                          }
                        ]
            ]
        ]
