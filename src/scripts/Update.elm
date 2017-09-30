module Update exposing (..)

import Actions exposing (Msg(ParseString))


update msg model =
    case msg of
        ParseString newValue ->
            ( { model | value = newValue }, Cmd.none )
