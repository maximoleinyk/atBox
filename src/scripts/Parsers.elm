module Parsers exposing (nothing, parse, space, word)

import ParsedResult exposing (ParsedResult)
import Regex exposing (HowMany(All))


word : List String -> List String
word string =
    case string of
        [] ->
            []

        first :: rest ->
            case first of
                _ ->
                    let
                        wordPattern =
                            Regex.regex "[^ @]"

                        foundResult =
                            Regex.find All wordPattern first

                        result =
                            List.map .match foundResult
                    in
                    if List.length result > 0 then
                        [ first ] ++ word rest
                    else
                        []


space : List String -> List String
space string =
    case string of
        [] ->
            []

        first :: rest ->
            case first of
                " " ->
                    [ first ] ++ space rest

                _ ->
                    []


nothing : List String -> List String
nothing string =
    []


parse : String -> (List String -> List String) -> ParsedResult
parse string parseFunction =
    let
        result =
            String.join "" (parseFunction (String.split "" string))

        parsedLength =
            String.length result

        newString =
            String.slice parsedLength (String.length string) string
    in
    if parsedLength == 0 then
        ParsedResult "" -1 ""
    else
        ParsedResult result parsedLength newString
