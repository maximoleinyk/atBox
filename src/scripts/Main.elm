module Main exposing (main)

import GlobalTypes exposing (Config, Model, Msg)
import Html exposing (programWithFlags)
import Init exposing (init)
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
