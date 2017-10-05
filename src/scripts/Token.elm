module Token exposing (..)

import FsmState exposing (FsmType)
import ParsedToken exposing (ParsedToken)


type alias Token =
    { state : FsmType
    , parsedToken : ParsedToken
    }
