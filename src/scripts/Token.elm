module Token exposing (..)

import FsmState exposing (FsmState)
import ParsedToken exposing (ParsedToken)


type alias Token =
    { state : FsmState
    , parsedToken : ParsedToken
    }
