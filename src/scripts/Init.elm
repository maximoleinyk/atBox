module Init exposing (getInitialModel, init)

import Config exposing (..)
import CursorPosition exposing (CursorPosition(NoContext))
import Json.Decode exposing (decodeValue, list)
import Maybe exposing (withDefault)
import Model exposing (..)
import Operator exposing (Operator)
import Ports exposing (inputChangeEvent)
import QueryField exposing (QueryField)
import QueryType exposing (QueryType, queryTypeDecoder)


init : Config -> ( Model, Cmd msg )
init flags =
    ( getInitialModel flags, inputChangeEvent "{ \"tokens\":[], \"lexemes\":[] }" )


getInitialModel : Config -> Model
getInitialModel flags =
    Model
        flags.id
        flags.label
        flags.placeholder
        ""
        flags.queryFields
        NoContext
        [ Operator "or" "or" True
        , Operator "and" "and" True
        , Operator "is" "is" False
        , Operator "is not" "is not" False
        , Operator "is either" "is either" False
        , Operator "is neither" "is neither" False
        , Operator "is in" "is in" False
        , Operator "is not in" "is not in" False
        ]
        ""
        ""
        "@"
