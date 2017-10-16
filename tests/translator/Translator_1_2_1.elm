module Translator_1_2_1 exposing (..)

import Expect
import GlobalTypes exposing (TranslatorOutput(EndOutput, NoOutput), TranslatorOutputValueType(Single))
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
            "@name is not Maksym"
    in
    describe "Translator.run"
        [ describe "is"
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
                            { field = "name"
                            , operator = "!="
                            , value = Single "Maksym"
                            }
                        )
            ]
        ]
