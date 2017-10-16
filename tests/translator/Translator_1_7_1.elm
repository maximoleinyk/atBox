module Translator_1_7_1 exposing (..)

import Expect
import GlobalTypes exposing (TranslatorOutput(AndOutput, EndOutput, NoOutput, OrOutput), TranslatorOutputValueType(Multiple, Single))
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
            "@forename is in (Maksym, Viktor) or @forename is not in (Alex, Julia)"
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
                        (OrOutput
                            { or =
                                [ EndOutput
                                    { field = "forename"
                                    , operator = "in"
                                    , value = Multiple [ "Maksym", "Viktor" ]
                                    }
                                , EndOutput
                                    { field = "forename"
                                    , operator = "not in"
                                    , value = Multiple [ "Alex", "Julia" ]
                                    }
                                ]
                            }
                        )
            ]
        ]
