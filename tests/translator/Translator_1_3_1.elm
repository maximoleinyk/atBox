module Translator_1_3_1 exposing (..)

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
            "@forename is either Maksym or Viktor"
    in
    describe "Translator.run"
        [ describe "is either"
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
                            , operator = "in"
                            , value = Multiple [ "Maksym", "Viktor" ]
                            }
                        )
            ]
        ]
