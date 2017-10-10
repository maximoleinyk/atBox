module Case_1_1_9 exposing (..)

import Expect
import MockModel exposing (getDefaultModel)
import Test exposing (Test, describe, test)
import Tokenizer exposing (TokenState(..))


suite : Test
suite =
    let
        testCase =
            "@nonexistingfield is Max"
    in
    describe "Tokenizer.run"
        [ describe "is"
            [ test testCase <|
                \_ ->
                    Expect.equal (Tokenizer.run testCase getDefaultModel)
                        [ { state = UnknownKeywordTerm
                          , parsedToken =
                                { string = "@nonexistingfield"
                                , length = 17
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
                        , { state = WordTerm
                          , parsedToken =
                                { string = "Max"
                                , length = 3
                                }
                          }
                        ]
            ]
        ]
