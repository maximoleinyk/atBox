module Utils exposing (..)

import Regex exposing (HowMany(All), escape, regex)


isNothing : Maybe a -> Bool
isNothing a =
    case a of
        Nothing ->
            True

        _ ->
            False


replace : String -> String -> String -> String
replace search substitution string =
    string
        |> Regex.replace All (regex (escape search)) (\_ -> substitution)
