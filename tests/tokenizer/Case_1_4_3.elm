module Case_1_4_3 exposing (..)

import Expect
import GlobalTypes exposing (TokenState(..))
import MockModel exposing (getDefaultModel)
import Test exposing (Test, describe, test)
import Tokenizer


suite : Test
suite =
    let
        testCase =
            "@forename is Maksym and @surname is either Ivanov or Petrov"
    in
    describe "Tokenizer.run"
        [ describe "and"
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
                        ]
            ]
        ]
