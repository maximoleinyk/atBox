module Utils exposing (..)

import Regex exposing (HowMany(All), escape, regex)


replace : String -> String -> String -> String
replace search substitution string =
    string
        |> Regex.replace All (regex (escape search)) (\_ -> substitution)
