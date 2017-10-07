module LexemeEncoder exposing (encode)

import Json.Encode exposing (list, object, string)
import Lexeme exposing (Lexeme)
import LexemeTokenEncoder exposing (encodeLexemeType)


encode : List Lexeme -> Json.Encode.Value
encode lexemes =
    let
        f =
            \l ->
                object
                    [ ( "lexemeType", encodeLexemeType l.lexemeType )
                    , ( "value", string l.value )
                    ]
    in
    list (List.map f lexemes)
