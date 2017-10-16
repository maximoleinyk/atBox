module Translator_1_4_1 exposing (..)

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
            "@forename is Maksym and @surname Oliinyk and @age is 26"
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
                                [ AndOutput
                                    { and =
                                        [ EndOutput
                                            { field = "forename"
                                            , operator = "=="
                                            , value = Single "Maksym"
                                            }
                                        , NoOutput
                                        ]
                                    }
                                , EndOutput
                                    { field = "age"
                                    , operator = "=="
                                    , value = Single "26"
                                    }
                                ]
                            }
                        )
            ]
        ]
