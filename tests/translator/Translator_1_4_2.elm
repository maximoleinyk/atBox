module Translator_1_4_2 exposing (..)

import Expect
import GlobalTypes exposing (TranslatorOutput(AndOutput, EndOutput, NoOutput), TranslatorOutputValueType(Multiple, Single))
import Lexer
import MockModel exposing (getDefaultModel)
import Parser
import Test exposing (Test, describe, test)
import Tokenizer
import Translator


suite : Test
suite =
    let
        testCase =
            "@age is 26 and @name is \"Maksym Oliinyk\""
    in
    describe "Translator.run"
        [ describe "is neither"
            [ test testCase <|
                \_ ->
                    let
                        model =
                            getDefaultModel

                        ( tokens, remainingStates ) =
                            Tokenizer.run testCase model

                        lexemes =
                            Lexer.run tokens model

                        ast =
                            Parser.run lexemes model
                    in
                    Expect.equal (Translator.run ast model)
                        (AndOutput
                            { and =
                                [ EndOutput
                                    { field = "age"
                                    , operator = "=="
                                    , value = Single "26"
                                    }
                                , EndOutput
                                    { field = "name"
                                    , operator = "=="
                                    , value = Single "Maksym Oliinyk"
                                    }
                                ]
                            }
                        )
            ]
        ]
