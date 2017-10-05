module Case_1_5_2 exposing (..)

import Expect
import FsmState exposing (FsmType(..))
import MockModel exposing (getDefaultModel)
import Test exposing (Test, describe, test)
import Tokenizer


suite : Test
suite =
    let
        testCase =
            "@forename is Maksym and @surname is either Ivanov or Petrov or @forename is Viktor and @surname is neither Sokolov nor Smirnov"
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
                                , remainingString = " is Maksym and @surname is either Ivanov or Petrov or @forename is Viktor and @surname is neither Sokolov nor Smirnov"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "is Maksym and @surname is either Ivanov or Petrov or @forename is Viktor and @surname is neither Sokolov nor Smirnov"
                                }
                          }
                        , { state = IsTerm
                          , parsedToken =
                                { string = "is"
                                , length = 2
                                , remainingString = " Maksym and @surname is either Ivanov or Petrov or @forename is Viktor and @surname is neither Sokolov nor Smirnov"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "Maksym and @surname is either Ivanov or Petrov or @forename is Viktor and @surname is neither Sokolov nor Smirnov"
                                }
                          }
                        , { state = WordTerm
                          , parsedToken =
                                { string = "Maksym"
                                , length = 6
                                , remainingString = " and @surname is either Ivanov or Petrov or @forename is Viktor and @surname is neither Sokolov nor Smirnov"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "and @surname is either Ivanov or Petrov or @forename is Viktor and @surname is neither Sokolov nor Smirnov"
                                }
                          }
                        , { state = AndTerm
                          , parsedToken =
                                { string = "and"
                                , length = 3
                                , remainingString = " @surname is either Ivanov or Petrov or @forename is Viktor and @surname is neither Sokolov nor Smirnov"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "@surname is either Ivanov or Petrov or @forename is Viktor and @surname is neither Sokolov nor Smirnov"
                                }
                          }
                        , { state = KeywordTerm
                          , parsedToken =
                                { string = "@surname"
                                , length = 8
                                , remainingString = " is either Ivanov or Petrov or @forename is Viktor and @surname is neither Sokolov nor Smirnov"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "is either Ivanov or Petrov or @forename is Viktor and @surname is neither Sokolov nor Smirnov"
                                }
                          }
                        , { state = IsTerm
                          , parsedToken =
                                { string = "is"
                                , length = 2
                                , remainingString = " either Ivanov or Petrov or @forename is Viktor and @surname is neither Sokolov nor Smirnov"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "either Ivanov or Petrov or @forename is Viktor and @surname is neither Sokolov nor Smirnov"
                                }
                          }
                        , { state = EitherTerm
                          , parsedToken =
                                { string = "either"
                                , length = 6
                                , remainingString = " Ivanov or Petrov or @forename is Viktor and @surname is neither Sokolov nor Smirnov"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "Ivanov or Petrov or @forename is Viktor and @surname is neither Sokolov nor Smirnov"
                                }
                          }
                        , { state = WordTerm
                          , parsedToken =
                                { string = "Ivanov"
                                , length = 6
                                , remainingString = " or Petrov or @forename is Viktor and @surname is neither Sokolov nor Smirnov"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "or Petrov or @forename is Viktor and @surname is neither Sokolov nor Smirnov"
                                }
                          }
                        , { state = OrTerm
                          , parsedToken =
                                { string = "or"
                                , length = 2
                                , remainingString = " Petrov or @forename is Viktor and @surname is neither Sokolov nor Smirnov"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "Petrov or @forename is Viktor and @surname is neither Sokolov nor Smirnov"
                                }
                          }
                        , { state = WordTerm
                          , parsedToken =
                                { string = "Petrov"
                                , length = 6
                                , remainingString = " or @forename is Viktor and @surname is neither Sokolov nor Smirnov"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "or @forename is Viktor and @surname is neither Sokolov nor Smirnov"
                                }
                          }
                        , { state = OrTerm
                          , parsedToken =
                                { string = "or"
                                , length = 2
                                , remainingString = " @forename is Viktor and @surname is neither Sokolov nor Smirnov"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "@forename is Viktor and @surname is neither Sokolov nor Smirnov"
                                }
                          }
                        , { state = KeywordTerm
                          , parsedToken =
                                { string = "@forename"
                                , length = 9
                                , remainingString = " is Viktor and @surname is neither Sokolov nor Smirnov"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "is Viktor and @surname is neither Sokolov nor Smirnov"
                                }
                          }
                        , { state = IsTerm
                          , parsedToken =
                                { string = "is"
                                , length = 2
                                , remainingString = " Viktor and @surname is neither Sokolov nor Smirnov"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "Viktor and @surname is neither Sokolov nor Smirnov"
                                }
                          }
                        , { state = WordTerm
                          , parsedToken =
                                { string = "Viktor"
                                , length = 6
                                , remainingString = " and @surname is neither Sokolov nor Smirnov"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "and @surname is neither Sokolov nor Smirnov"
                                }
                          }
                        , { state = AndTerm
                          , parsedToken =
                                { string = "and"
                                , length = 3
                                , remainingString = " @surname is neither Sokolov nor Smirnov"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "@surname is neither Sokolov nor Smirnov"
                                }
                          }
                        , { state = KeywordTerm
                          , parsedToken =
                                { string = "@surname"
                                , length = 8
                                , remainingString = " is neither Sokolov nor Smirnov"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "is neither Sokolov nor Smirnov"
                                }
                          }
                        , { state = IsTerm
                          , parsedToken =
                                { string = "is"
                                , length = 2
                                , remainingString = " neither Sokolov nor Smirnov"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "neither Sokolov nor Smirnov"
                                }
                          }
                        , { state = NeitherTerm
                          , parsedToken =
                                { string = "neither"
                                , length = 7
                                , remainingString = " Sokolov nor Smirnov"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "Sokolov nor Smirnov"
                                }
                          }
                        , { state = WordTerm
                          , parsedToken =
                                { string = "Sokolov"
                                , length = 7
                                , remainingString = " nor Smirnov"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "nor Smirnov"
                                }
                          }
                        , { state = NorTerm
                          , parsedToken =
                                { string = "nor"
                                , length = 3
                                , remainingString = " Smirnov"
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                , remainingString = "Smirnov"
                                }
                          }
                        , { state = WordTerm
                          , parsedToken =
                                { string = "Smirnov"
                                , length = 7
                                , remainingString = ""
                                }
                          }
                        ]
            ]
        ]
