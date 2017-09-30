module Main exposing (..)

import Actions exposing (..)
import Config exposing (Config)
import Html
import Init exposing (init)
import Maybe exposing (withDefault)
import Model exposing (Model)
import Update exposing (update)
import View exposing (view)


main : Program Config Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions model =
    Sub.none
