module Case_1_1_3 exposing (..)

import Expect
import FsmState exposing (FsmType(..))
import MockModel exposing (getDefaultModel)
import Test exposing (Test, describe, test)
import Tokenizer


suite : Test
suite =
    let
        testCase =
            "find a person whose @name is \"Maksym Oliinyk\""
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
                                , remainingString = " a person whose @name is \"Maksym Oliinyk\""
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "a person whose @name is \"Maksym Oliinyk\""
                                }
                          }
                        , { state = WordTerm
                          , parsedToken =
                                { string = "a"
                                , length = 1
                                , remainingString = " person whose @name is \"Maksym Oliinyk\""
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "person whose @name is \"Maksym Oliinyk\""
                                }
                          }
                        , { state = WordTerm
                          , parsedToken =
                                { string = "person"
                                , length = 6
                                , remainingString = " whose @name is \"Maksym Oliinyk\""
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "whose @name is \"Maksym Oliinyk\""
                                }
                          }
                        , { state = WordTerm
                          , parsedToken =
                                { string = "whose"
                                , length = 5
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
