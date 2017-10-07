module Case_1_6_2 exposing (..)

import Expect
import MockModel exposing (getDefaultModel)
import Test exposing (Test, describe, test)
import Tokenizer exposing (TokenState(..))


suite : Test
suite =
    let
        testCase =
            "@forename is in ()"
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
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                }
                          }
                        , { state = IsTerm
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
                        , { state = InTerm
                          , parsedToken =
                                { string = "in"
                                , length = 2
                                }
                          }
                        , { state = SpaceTerm
                          , parsedToken =
                                { string = " "
                                , length = 1
                                }
                          }
                        , { state = OpenParenthesisTerm
                          , parsedToken =
                                { string = "("
                                , length = 1
                                }
                          }
                        , { state = CloseParenthesisTerm
                          , parsedToken =
                                { string = ")"
                                , length = 1
                                }
                          }
                        ]
            ]
        ]
