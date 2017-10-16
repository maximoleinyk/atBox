module Translator_1_5_2 exposing (..)

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
            "@forename is Maksym and @surname is either Ivanov or Petrov or @forename is Viktor and @surname is neither Sokolov nor Smirnov"
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
                                [ AndOutput
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
                                , AndOutput
                                    { and =
                                        [ EndOutput
                                            { field = "forename"
                                            , operator = "=="
                                            , value = Single "Viktor"
                                            }
                                        , EndOutput
                                            { field = "surname"
                                            , operator = "not in"
                                            , value = Multiple [ "Sokolov", "Smirnov" ]
                                            }
                                        ]
                                    }
                                ]
                            }
                        )
            ]
        ]
