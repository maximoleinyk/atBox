port module Ports exposing (..)


port inputChangeEvent : String -> Cmd msg


port keyDownEvent : String -> Cmd msg


port caretPosition : (Int -> msg) -> Sub msg
