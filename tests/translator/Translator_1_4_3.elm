module Translator_1_4_3 exposing (..)

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
            "@forename is Maksym and @surname is either Ivanov or Petrov"
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
                                    { field = "forename"
                                    , operator = "=="
                                    , value = Single "Maksym"
                                    }
                                , EndOutput
                                    { field = "surname"
                                    , operator = "in"
                                    , value = Multiple [ "Ivanov", "Petrov" ]
                                    }
                                ]
                            }
                        )
            ]
        ]
