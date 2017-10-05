module TokenEncoder exposing (..)

import FsmState exposing (FsmType)
import FsmStateEncoder exposing (encodeState)
import Json.Encode exposing (Value, encode, int, list, object, string)
import ParsedToken exposing (ParsedToken)
import ParsedTokenEncoder exposing (encodeParsedToken)
import Token exposing (Token)


encodeTokens : List Token -> String
encodeTokens tokens =
    let
        f =
            \t ->
                object
                    [ ( "state", encodeState t.state )
                    , ( "parsedToken", encodeParsedToken t.parsedToken )
                    ]
    in
    encode 2 (list (List.map f tokens))
