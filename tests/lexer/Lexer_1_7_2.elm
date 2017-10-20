module Lexer_1_7_2 exposing (..)

import Expect
import GlobalTypes exposing (LexemeState(..), OperatorType(..))
import Lexer
import MockModel exposing (getDefaultModel)
import Test exposing (Test, describe, test)
import Tokenizer


suite : Test
suite =
    let
        testCase =
            "((@name is either Max or Joe) or (@surname is neither Oliinyk nor Doe)) and ((@age is not 27) or (@forename is Oliinyk)) or @surname is not Smirnov"
    in
    describe "Lexer.run"
        [ describe "is"
            [ test testCase <|
                \_ ->
                    let
                        model =
                            getDefaultModel

                        ( tokens, remainingStates ) =
                            Tokenizer.run testCase model
                    in
                    Expect.equal (Lexer.run tokens model)
                        [ { state = LeftParenthesis, value = "(", index = 0 }
                        , { state = LeftParenthesis, value = "(", index = 1 }
                        , { state = Field, value = "@name", index = 2 }
                        , { state = Operator IsEitherType, value = "is either", index = 8 }
                        , { state = LexemeValue, value = " Max or Joe", index = 17 }
                        , { state = RightParenthesis, value = ")", index = 28 }
                        , { state = Joiner, value = "or", index = 30 }
                        , { state = LeftParenthesis, value = "(", index = 33 }
                        , { state = Field, value = "@surname", index = 34 }
                        , { state = Operator IsNeitherType, value = "is neither", index = 43 }
                        , { state = LexemeValue, value = " Oliinyk nor Doe", index = 53 }
                        , { state = RightParenthesis, value = ")", index = 69 }
                        , { state = RightParenthesis, value = ")", index = 70 }
                        , { state = Joiner, value = "and", index = 72 }
                        , { state = LeftParenthesis, value = "(", index = 76 }
                        , { state = LeftParenthesis, value = "(", index = 77 }
                        , { state = Field, value = "@age", index = 78 }
                        , { state = Operator IsNotType, value = "is not", index = 83 }
                        , { state = LexemeValue, value = "27", index = 90 }
                        , { state = RightParenthesis, value = ")", index = 92 }
                        , { state = Joiner, value = "or", index = 94 }
                        , { state = LeftParenthesis, value = "(", index = 97 }
                        , { state = Field, value = "@forename", index = 98 }
                        , { state = Operator IsType, value = "is", index = 108 }
                        , { state = LexemeValue, value = "Oliinyk", index = 111 }
                        , { state = RightParenthesis, value = ")", index = 118 }
                        , { state = RightParenthesis, value = ")", index = 119 }
                        , { state = Joiner, value = "or", index = 121 }
                        , { state = Field, value = "@surname", index = 124 }
                        , { state = Operator IsNotType, value = "is not", index = 133 }
                        , { state = LexemeValue, value = "Smirnov", index = 140 }
                        ]
            ]
        ]
