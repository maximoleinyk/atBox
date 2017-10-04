module FsmStateEncoder exposing (..)

import FsmState exposing (FsmState)
import Json.Encode exposing (Value, string)


encodeState : FsmState -> Value
encodeState =
    \state -> string (toString state)
