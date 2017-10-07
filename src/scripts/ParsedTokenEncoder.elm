module ParsedTokenEncoder exposing (..)

import Json.Encode exposing (Value, int, object, string)
import ParsedToken exposing (ParsedToken)


encodeParsedToken : ParsedToken -> Value
encodeParsedToken parsedToken =
    object
        [ ( "string", string parsedToken.string )
        , ( "length", int parsedToken.length )
        ]
