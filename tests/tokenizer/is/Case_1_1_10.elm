module Case_1_1_10 exposing (..)

import Expect
import FsmState exposing (FsmType(..))
import MockModel exposing (getDefaultModel)
import Test exposing (Test, describe, test)
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
                                , remainingString = " could be or is Maksym Oliinyk"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "could be or is Maksym Oliinyk"
                                }
                          }
                        , { state = WordTerm
                          , parsedToken =
                                { string = "could"
                                , length = 5
                                , remainingString = " be or is Maksym Oliinyk"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "be or is Maksym Oliinyk"
                                }
                          }
                        , { state = WordTerm
                          , parsedToken =
                                { string = "be"
                                , length = 2
                                , remainingString = " or is Maksym Oliinyk"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "or is Maksym Oliinyk"
                                }
                          }
                        , { state = WordTerm
                          , parsedToken =
                                { string = "or"
                                , length = 2
                                , remainingString = " is Maksym Oliinyk"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "is Maksym Oliinyk"
                                }
                          }
                        , { state = WordTerm
                          , parsedToken =
                                { string = "is"
                                , length = 2
                                , remainingString = " Maksym Oliinyk"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "Maksym Oliinyk"
                                }
                          }
                        , { state = WordTerm
                          , parsedToken =
                                { string = "Maksym"
                                , length = 6
                                , remainingString = " Oliinyk"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "Oliinyk"
                                }
                          }
                        , { state = WordTerm
                          , parsedToken =
                                { string = "Oliinyk"
                                , length = 7
                                , remainingString = ""
                                }
                          }
                        ]
            ]
        ]
