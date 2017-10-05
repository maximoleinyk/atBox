module Case_1_2_2 exposing (..)

import Expect
import FsmState exposing (FsmType(..))
import MockModel exposing (getDefaultModel)
import Test exposing (Test, describe, test)
import Tokenizer


suite : Test
suite =
    let
        testCase =
            "@name is something not something Maksym"
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
                                , remainingString = " is something not something Maksym"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "is something not something Maksym"
                                }
                          }
                        , { state = IsTerm
                          , parsedToken =
                                { string = "is"
                                , length = 2
                                , remainingString = " something not something Maksym"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "something not something Maksym"
                                }
                          }
                        , { state = WordTerm
                          , parsedToken =
                                { string = "something"
                                , length = 9
                                , remainingString = " not something Maksym"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "not something Maksym"
                                }
                          }
                        , { state = WordTerm
                          , parsedToken =
                                { string = "not"
                                , length = 3
                                , remainingString = " something Maksym"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "something Maksym"
                                }
                          }
                        , { state = WordTerm
                          , parsedToken =
                                { string = "something"
                                , length = 9
                                , remainingString = " Maksym"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "Maksym"
                                }
                          }
                        , { state = WordTerm
                          , parsedToken =
                                { string = "Maksym"
                                , length = 6
                                , remainingString = ""
                                }
                          }
                        ]
            ]
        ]
