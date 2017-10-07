port module Ports exposing (..)


port emitData : String -> Cmd msg


port getCursorPosition : String -> Cmd msg


port setCursorPosition : (Int -> msg) -> Sub msg
