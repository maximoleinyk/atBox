module ParsedToken exposing (..)

import FsmState exposing (FsmState)


type alias ParsedToken =
    { state : FsmState
    , string : String
    }
