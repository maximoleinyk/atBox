module Init exposing (getInitialModel, init)

import Config exposing (..)
import CursorPosition exposing (CursorPosition(NoContext))
import Encoders exposing (encodeFsmResponse)
import FsmResponse exposing (FsmResponse)
import Json.Decode exposing (decodeValue, list)
import Maybe exposing (withDefault)
import Model exposing (..)
import Operator exposing (Operator)
import Parser exposing (AST(Nil))
import Ports exposing (emitData)
import QueryField exposing (QueryField)
import QueryType exposing (QueryType, queryTypeDecoder)


init : Config -> ( Model, Cmd msg )
init flags =
    let
        model =
            getInitialModel flags

        message =
            encodeFsmResponse (FsmResponse [] [] Nil)

        command =
            emitData message
    in
    ( model, command )


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
        0
