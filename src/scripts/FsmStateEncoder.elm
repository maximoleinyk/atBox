module FsmStateEncoder exposing (..)

import FsmState exposing (FsmType)
import Json.Encode exposing (Value, string)


encodeState : FsmType -> Value
encodeState =
    \state -> string (toString state)
