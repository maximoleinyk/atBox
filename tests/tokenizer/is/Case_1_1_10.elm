module Case_1_1_10 exposing (..)

import Expect
import MockModel exposing (getDefaultModel)
import Test exposing (Test, describe, test)
import TokenState exposing (TokenState(..))
import Tokenizer


suite : Test
suite =
    let
        testCase =
            "@name could be or is Maksym Oliinyk"
    in
    describe "Tokenizer.run"
        [ describe "is"
            [ test testCase <|
                \_ ->
                    Expect.equal (Tokenizer.run testCase getDefaultModel)
                        [ { state = KeywordTerm
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
                        , { state = WordTerm
                          , parsedToken =
                                { string = "could"
                                , length = 5
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
                                { string = "be"
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
                                { string = "or"
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
