module Case_1_4_3 exposing (..)

import Expect
import MockModel exposing (getDefaultModel)
import Test exposing (Test, describe, test)
import TokenState exposing (TokenState(..))
import Tokenizer


suite : Test
suite =
    let
        testCase =
            "@forename is Maksym and @surname is either Ivanov or Petrov"
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
                                , remainingString = " is Maksym and @surname is either Ivanov or Petrov"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "is Maksym and @surname is either Ivanov or Petrov"
                                }
                          }
                        , { state = IsTerm
                          , parsedToken =
                                { string = "is"
                                , length = 2
                                , remainingString = " Maksym and @surname is either Ivanov or Petrov"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "Maksym and @surname is either Ivanov or Petrov"
                                }
                          }
                        , { state = WordTerm
                          , parsedToken =
                                { string = "Maksym"
                                , length = 6
                                , remainingString = " and @surname is either Ivanov or Petrov"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "and @surname is either Ivanov or Petrov"
                                }
                          }
                        , { state = AndTerm
                          , parsedToken =
                                { string = "and"
                                , length = 3
                                , remainingString = " @surname is either Ivanov or Petrov"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "@surname is either Ivanov or Petrov"
                                }
                          }
                        , { state = KeywordTerm
                          , parsedToken =
                                { string = "@surname"
                                , length = 8
                                , remainingString = " is either Ivanov or Petrov"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "is either Ivanov or Petrov"
                                }
                          }
                        , { state = IsTerm
                          , parsedToken =
                                { string = "is"
                                , length = 2
                                , remainingString = " either Ivanov or Petrov"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "either Ivanov or Petrov"
                                }
                          }
                        , { state = EitherTerm
                          , parsedToken =
                                { string = "either"
                                , length = 6
                                , remainingString = " Ivanov or Petrov"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "Ivanov or Petrov"
                                }
                          }
                        , { state = WordTerm
                          , parsedToken =
                                { string = "Ivanov"
                                , length = 6
                                , remainingString = " or Petrov"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "or Petrov"
                                }
                          }
                        , { state = OrTerm
                          , parsedToken =
                                { string = "or"
                                , length = 2
                                , remainingString = " Petrov"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "Petrov"
                                }
                          }
                        , { state = WordTerm
                          , parsedToken =
                                { string = "Petrov"
                                , length = 6
                                , remainingString = ""
                                }
                          }
                        ]
            ]
        ]
