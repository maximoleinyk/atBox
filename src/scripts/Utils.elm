module Utils exposing (..)


isEnter : Int -> Bool
isEnter =
    \code ->
        if code == 13 then
            True
        else
            False
