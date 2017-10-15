module Case_1_5_2 exposing (..)

import Expect
import GlobalTypes exposing (TokenState(..))
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
        [ describe "or"
            [ test testCase <|
                \_ ->
                    let
                        ( tokens, remainingStates ) =
                            Tokenizer.run testCase getDefaultModel
                    in
                    Expect.equal tokens
                        [ { state = KeywordTerm
                          , value = "@forename"
                          , index = 0
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 9
                          }
                        , { state = IsTerm
                          , value = "is"
                          , index = 10
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 12
                          }
                        , { state = WordTerm
                          , value = "Maksym"
                          , index = 13
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 19
                          }
                        , { state = AndTerm
                          , value = "and"
                          , index = 20
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 23
                          }
                        , { state = KeywordTerm
                          , value = "@surname"
                          , index = 24
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 32
                          }
                        , { state = IsEitherTerm
                          , value = "is either"
                          , index = 33
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 42
                          }
                        , { state = WordTerm
                          , value = "Ivanov"
                          , index = 43
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 49
                          }
                        , { state = EitherOrTerm
                          , value = "or"
                          , index = 50
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 52
                          }
                        , { state = WordTerm
                          , value = "Petrov"
                          , index = 53
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 59
                          }
                        , { state = OrTerm
                          , value = "or"
                          , index = 60
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 62
                          }
                        , { state = KeywordTerm
                          , value = "@forename"
                          , index = 63
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 72
                          }
                        , { state = IsTerm
                          , value = "is"
                          , index = 73
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 75
                          }
                        , { state = WordTerm
                          , value = "Viktor"
                          , index = 76
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 82
                          }
                        , { state = AndTerm
                          , value = "and"
                          , index = 83
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 86
                          }
                        , { state = KeywordTerm
                          , value = "@surname"
                          , index = 87
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 95
                          }
                        , { state = IsNeitherTerm
                          , value = "is neither"
                          , index = 96
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 106
                          }
                        , { state = WordTerm
                          , value = "Sokolov"
                          , index = 107
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 114
                          }
                        , { state = NeitherNorTerm
                          , value = "nor"
                          , index = 115
                          }
                        , { state = SpaceTerm
                          , value = " "
                          , index = 118
                          }
                        , { state = WordTerm
                          , value = "Smirnov"
                          , index = 119
                          }
                        ]
            ]
        ]
