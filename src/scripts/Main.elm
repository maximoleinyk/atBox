module Main exposing (main)

import Actions exposing (Msg)
import Config exposing (Config)
import Html exposing (programWithFlags)
import Init exposing (init)
import Model exposing (Model)
import Subscriptions exposing (subscriptions)
import Update exposing (update)
import View exposing (view)


main : Program Config Model Msg
main =
    programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
