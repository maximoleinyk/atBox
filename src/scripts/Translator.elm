module Translator exposing (TranslatorOutput(..), run)

import Model exposing (Model)
import Parser exposing (AST)


type TranslatorOutput
    = Output String


run : AST -> Model -> TranslatorOutput
run root model =
    Output "[]"
