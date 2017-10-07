module Token exposing (..)

import ParsedToken exposing (ParsedToken)
import TokenState exposing (TokenState)


type alias Token =
    { state : TokenState
    , parsedToken : ParsedToken
    }
