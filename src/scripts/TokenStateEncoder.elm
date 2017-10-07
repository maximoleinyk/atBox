module TokenStateEncoder exposing (..)

import Json.Encode exposing (Value, string)
import TokenState exposing (TokenState)


encodeState : TokenState -> Value
encodeState =
    \state -> string (toString state)
