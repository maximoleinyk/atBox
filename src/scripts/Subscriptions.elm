module Subscriptions exposing (..)

import GlobalTypes exposing (Model, Msg(UpdateCursorPosition))
import Ports exposing (setCursorPosition)


subscriptions : Model -> Sub Msg
subscriptions model =
    setCursorPosition UpdateCursorPosition
