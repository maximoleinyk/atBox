module Tokenizer_1_7_2 exposing (..)

import Expect
import GlobalTypes exposing (TokenState(..))
import MockModel exposing (getDefaultModel)
import Test exposing (Test, describe, test)
import Tokenizer


suite : Test
suite =
    let
        testCase =
            "((@name is either Max or Joe) or (@surname is neither Oliinyk nor Doe)) and ((@age is not 27) or (@forename is Oliinyk)) or @surname is not Smirnov"
    in
    describe "Tokenizer.run"
        [ describe "and / or"
            [ test testCase <|
                \_ ->
                    let
                        ( tokens, remainingStates ) =
                            Tokenizer.run testCase getDefaultModel
                    in
                    Expect.equal tokens
                        [ { state = OpenParenthesisTerm, value = "(", index = 0 }
                        , { state = OpenParenthesisTerm, value = "(", index = 1 }
                        , { state = KeywordTerm, value = "@name", index = 2 }
                        , { state = SpaceTerm, value = " ", index = 7 }
                        , { state = IsEitherTerm, value = "is either", index = 8 }
                        , { state = SpaceTerm, value = " ", index = 17 }
                        , { state = WordTerm, value = "Max", index = 18 }
                        , { state = SpaceTerm, value = " ", index = 21 }
                        , { state = EitherOrTerm, value = "or", index = 22 }
                        , { state = SpaceTerm, value = " ", index = 24 }
                        , { state = WordTerm, value = "Joe", index = 25 }
                        , { state = CloseParenthesisTerm, value = ")", index = 28 }
                        , { state = SpaceTerm, value = " ", index = 29 }
                        , { state = OrTerm, value = "or", index = 30 }
                        , { state = SpaceTerm, value = " ", index = 32 }
                        , { state = OpenParenthesisTerm, value = "(", index = 33 }
                        , { state = KeywordTerm, value = "@surname", index = 34 }
                        , { state = SpaceTerm, value = " ", index = 42 }
                        , { state = IsNeitherTerm, value = "is neither", index = 43 }
                        , { state = SpaceTerm, value = " ", index = 53 }
                        , { state = WordTerm, value = "Oliinyk", index = 54 }
                        , { state = SpaceTerm, value = " ", index = 61 }
                        , { state = NeitherNorTerm, value = "nor", index = 62 }
                        , { state = SpaceTerm, value = " ", index = 65 }
                        , { state = WordTerm, value = "Doe", index = 66 }
                        , { state = CloseParenthesisTerm, value = ")", index = 69 }
                        , { state = CloseParenthesisTerm, value = ")", index = 70 }
                        , { state = SpaceTerm, value = " ", index = 71 }
                        , { state = AndTerm, value = "and", index = 72 }
                        , { state = SpaceTerm, value = " ", index = 75 }
                        , { state = OpenParenthesisTerm, value = "(", index = 76 }
                        , { state = OpenParenthesisTerm, value = "(", index = 77 }
                        , { state = KeywordTerm, value = "@age", index = 78 }
                        , { state = SpaceTerm, value = " ", index = 82 }
                        , { state = IsNotTerm, value = "is not", index = 83 }
                        , { state = SpaceTerm, value = " ", index = 89 }
                        , { state = WordTerm, value = "27", index = 90 }
                        , { state = CloseParenthesisTerm, value = ")", index = 92 }
                        , { state = SpaceTerm, value = " ", index = 93 }
                        , { state = OrTerm, value = "or", index = 94 }
                        , { state = SpaceTerm, value = " ", index = 96 }
                        , { state = OpenParenthesisTerm, value = "(", index = 97 }
                        , { state = KeywordTerm, value = "@forename", index = 98 }
                        , { state = SpaceTerm, value = " ", index = 107 }
                        , { state = IsTerm, value = "is", index = 108 }
                        , { state = SpaceTerm, value = " ", index = 110 }
                        , { state = WordTerm, value = "Oliinyk", index = 111 }
                        , { state = CloseParenthesisTerm, value = ")", index = 118 }
                        , { state = CloseParenthesisTerm, value = ")", index = 119 }
                        , { state = SpaceTerm, value = " ", index = 120 }
                        , { state = OrTerm, value = "or", index = 121 }
                        , { state = SpaceTerm, value = " ", index = 123 }
                        , { state = KeywordTerm, value = "@surname", index = 124 }
                        , { state = SpaceTerm, value = " ", index = 132 }
                        , { state = IsNotTerm, value = "is not", index = 133 }
                        , { state = SpaceTerm, value = " ", index = 139 }
                        , { state = WordTerm, value = "Smirnov", index = 140 }
                        ]
            ]
        ]
