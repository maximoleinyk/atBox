module LexemeTokenEncoder exposing (encodeLexemeType)

import Json.Encode exposing (string)
import LexemeType exposing (LexemeType)


encodeLexemeType : LexemeType -> Json.Encode.Value
encodeLexemeType =
    \lexemeType -> string (toString lexemeType)
