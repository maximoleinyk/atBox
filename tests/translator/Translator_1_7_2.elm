module Translator_1_7_2 exposing (..)

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
            "((@name is either Max or Joe) or (@surname is neither Oliinyk nor Doe)) and ((@age is not 27) or (@forename is Oliinyk)) or @surname is not Smirnov"
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
                                        [ OrOutput
                                            { or =
                                                [ EndOutput
                                                    { field = "name"
                                                    , operator = "in"
                                                    , value = Multiple [ "Max", "Joe" ]
                                                    }
                                                , EndOutput
                                                    { field = "surname"
                                                    , operator = "not in"
                                                    , value = Multiple [ "Oliinyk", "Doe" ]
                                                    }
                                                ]
                                            }
                                        , OrOutput
                                            { or =
                                                [ EndOutput
                                                    { field = "age"
                                                    , operator = "!="
                                                    , value = Single "27"
                                                    }
                                                , EndOutput
                                                    { field = "forename"
                                                    , operator = "=="
                                                    , value = Single "Oliinyk"
                                                    }
                                                ]
                                            }
                                        ]
                                    }
                                , EndOutput
                                    { field = "surname"
                                    , operator = "!="
                                    , value = Single "Smirnov"
                                    }
                                ]
                            }
                        )
            ]
        ]
