module Case_1_1_5 exposing (..)

import Expect
import FsmState exposing (FsmType(..))
import MockModel exposing (getDefaultModel)
import Test exposing (Test, describe, test)
import Tokenizer


suite : Test
suite =
    let
        testCase =
            "find a person whose name Maksym"
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
                                , remainingString = " a person whose name Maksym"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "a person whose name Maksym"
                                }
                          }
                        , { state = WordTerm
                          , parsedToken =
                                { string = "a"
                                , length = 1
                                , remainingString = " person whose name Maksym"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "person whose name Maksym"
                                }
                          }
                        , { state = WordTerm
                          , parsedToken =
                                { string = "person"
                                , length = 6
                                , remainingString = " whose name Maksym"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "whose name Maksym"
                                }
                          }
                        , { state = WordTerm
                          , parsedToken =
                                { string = "whose"
                                , length = 5
                                , remainingString = " name Maksym"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "name Maksym"
                                }
                          }
                        , { state = WordTerm
                          , parsedToken =
                                { string = "name"
                                , length = 4
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
