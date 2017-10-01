module Parsers exposing (noParser, parseSpace, parseWord)

import ParsedResult exposing (ParsedResult)
import Regex exposing (HowMany(All))


doParseWord : List String -> List String
doParseWord string =
    case string of
        [] ->
            []

        first :: rest ->
            case first of
                _ ->
                    let
                        wordPattern =
                            Regex.regex "\\S"

                        foundResult =
                            Regex.find All wordPattern first

                        result =
                            List.map .match foundResult
                    in
                    if List.length result > 0 then
                        [ first ] ++ doParseWord rest
                    else
                        []


doParseSpace : List String -> List String
doParseSpace string =
    case string of
        [] ->
            []

        first :: rest ->
            case first of
                " " ->
                    [ first ] ++ doParseSpace rest

                _ ->
                    []


noParser : String -> ParsedResult
noParser string =
    ParsedResult "" -1 ""


genericParse : String -> (List String -> List String) -> ParsedResult
genericParse string parseFunction =
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


parseSpace : String -> ParsedResult
parseSpace string =
    genericParse string doParseSpace


parseWord : String -> ParsedResult
parseWord string =
    genericParse string doParseWord
