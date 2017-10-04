module TokenizerTest exposing (..)

import Expect
import FsmState exposing (FsmState(..))
import Lexer
import MockModel exposing (getDefaultModel)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Tokenizer.evaluate"
        [ describe "@name is \"Maksym Oliinyk\""
            [ test "has no effect on a palindrome" <|
                \_ ->
                    let
                        string =
                            "@name is \"Maksym Oliinyk\""

                        actualResult =
                            Lexer.evaluate string getDefaultModel

                        expectedResult =
                            [ { state = KeywordTerm
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
                    in
                    Expect.equal actualResult expectedResult
            ]
        ]
