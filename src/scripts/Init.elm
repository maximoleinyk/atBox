module Init exposing (getInitialModel, init)

import Config exposing (..)
import CursorPosition exposing (CursorPosition(NoContext))
import Encoders exposing (encodeFsmResponse)
import FsmResponse exposing (FsmResponse)
import Json.Decode exposing (decodeValue, list)
import Maybe exposing (withDefault)
import Model exposing (..)
import Operator exposing (Operator)
import Parser exposing (AST(Null))
import Ports exposing (emitData)
import QueryField exposing (QueryField)
import QueryType exposing (QueryType, queryTypeDecoder)
import Translator exposing (Output(NoOutput))


init : Config -> ( Model, Cmd msg )
init flags =
    let
        model =
            getInitialModel flags

        message =
            encodeFsmResponse (FsmResponse [] [] Null NoOutput)

        command =
            emitData message
    in
    ( model, command )


getInitialModel : Config -> Model
getInitialModel config =
    Model
        config.id
        config.label
        config.placeholder
        config.value
        config.queryFields
        NoContext
        [ Operator "or" "or"
        , Operator "and" "and"
        , Operator "is" "is"
        , Operator "is not" "is not"
        , Operator "is either" "is either"
        , Operator "is neither" "is neither"
        , Operator "is in" "is in"
        , Operator "is not in" "is not in"
        ]
        ""
        "@"
        0
