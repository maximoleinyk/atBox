module Translator_1_3_2 exposing (..)

import Expect
import GlobalTypes exposing (TranslatorOutput(EndOutput, NoOutput), TranslatorOutputValueType(Multiple, Single))
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
            "@forename is neither Maksym nor Viktor"
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
                        (EndOutput
                            { field = "forename"
                            , operator = "not in"
                            , value = Multiple [ "Maksym", "Viktor" ]
                            }
                        )
            ]
        ]
