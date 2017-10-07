module Lexeme exposing (..)

import LexemeType exposing (LexemeType)


type alias Lexeme =
    { lexemeType : LexemeType
    , value : String
    }
