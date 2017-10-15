module Init exposing (getInitialModel, init)

import Encoders exposing (encodeFsmResponse)
import GlobalTypes exposing (AST(Null), Config, CursorContext(NoContext), FsmResponse, Model, TranslatorOutput(NoOutput))
import Json.Decode exposing (decodeValue, list)
import Maybe exposing (withDefault)
import Ports exposing (emitData)


init : Config -> ( Model, Cmd msg )
init flags =
    let
        model =
            getInitialModel flags

        message =
            encodeFsmResponse (FsmResponse [] [] Null NoOutput "")

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
        ""
        "@"
        0
        False
