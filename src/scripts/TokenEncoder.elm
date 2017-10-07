module TokenEncoder exposing (encode)

import Json.Encode exposing (Value, encode, int, list, object, string)
import ParsedToken exposing (ParsedToken)
import ParsedTokenEncoder exposing (encodeParsedToken)
import Token exposing (Token)
import TokenState exposing (TokenState)
import TokenStateEncoder exposing (encodeState)


encode : List Token -> Json.Encode.Value
encode tokens =
    let
        f =
            \t ->
                object
                    [ ( "state", encodeState t.state )
                    , ( "parsedToken", encodeParsedToken t.parsedToken )
                    ]
    in
    list (List.map f tokens)
