module Case_1_6_3 exposing (..)

import Expect
import MockModel exposing (getDefaultModel)
import Test exposing (Test, describe, test)
import TokenState exposing (TokenState(..))
import Tokenizer


suite : Test
suite =
    let
        testCase =
            "@forename is in"
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
                        ]
            ]
        ]
