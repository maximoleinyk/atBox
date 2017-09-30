module Init exposing (..)

import Config exposing (..)
import Maybe exposing (withDefault)
import Model exposing (..)


init : Config -> ( Model, Cmd msg )
init flags =
    ( getInitialModel flags, Cmd.none )


getInitialModel : Config -> Model
getInitialModel flags =
    { id = flags.id
    , label = flags.label
    , placeholder = flags.placeholder
    , value = ""
    }
