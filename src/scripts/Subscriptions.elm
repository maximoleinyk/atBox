module Subscriptions exposing (..)

import Actions exposing (Msg(UpdateCaretIndex))
import Model exposing (Model)
import Ports exposing (setCursorPosition)


subscriptions : Model -> Sub Msg
subscriptions model =
    setCursorPosition UpdateCaretIndex
