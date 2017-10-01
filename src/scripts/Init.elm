module Init exposing (..)

import Config exposing (..)
import CursorPosition exposing (CursorPosition(..))
import Json.Decode exposing (decodeValue, list)
import Maybe exposing (withDefault)
import Model exposing (..)
import Operator exposing (Operator)
import QueryField exposing (QueryField)
import QueryType exposing (QueryType, queryTypeDecoder)


init : Config -> ( Model, Cmd msg )
init flags =
    ( getInitialModel flags, Cmd.none )


getInitialModel : Config -> Model
getInitialModel flags =
    { id = flags.id
    , label = flags.label
    , placeholder = flags.placeholder
    , cursorPosition = NoContext
    , queryFields = flags.queryFields
    , value = ""
    , operators =
        [ Operator "or" "or" True
        , Operator "and" "and" True
        , Operator "is" "is" False
        , Operator "is not" "is not" False
        , Operator "is either" "is either" False
        , Operator "is neither" "is neither" False
        , Operator "is in" "is in" False
        , Operator "is not in" "is not in" False
        ]
    , currentToken = ""
    , selectedItem = ""
    }
