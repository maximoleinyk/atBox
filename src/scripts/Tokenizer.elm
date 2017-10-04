module Tokenizer exposing (..)

import Model exposing (Model)
import ParsedToken exposing (ParsedToken)
import Regex exposing (HowMany(All), Regex)


regexTokenizer : String -> Regex -> String
regexTokenizer string pattern =
    let
        patternMatches =
            Regex.find All pattern string

        result =
            List.head (List.map .match patternMatches)
    in
    case result of
        Just parsedString ->
            parsedString

        Nothing ->
            ""


closeParenthesis : String -> Model -> String
closeParenthesis string model =
    regexTokenizer string (Regex.regex "^(\\))")


openParenthesis : String -> Model -> String
openParenthesis string model =
    regexTokenizer string (Regex.regex "^(\\()")


inTerm : String -> Model -> String
inTerm string model =
    regexTokenizer string (Regex.caseInsensitive (Regex.regex "^(in)"))


comma : String -> Model -> String
comma string model =
    regexTokenizer string (Regex.regex "^(,)")


is : String -> Model -> String
is string model =
    regexTokenizer string (Regex.caseInsensitive (Regex.regex "^(is)"))


either : String -> Model -> String
either string model =
    regexTokenizer string (Regex.caseInsensitive (Regex.regex "^(either)"))


neither : String -> Model -> String
neither string model =
    regexTokenizer string (Regex.caseInsensitive (Regex.regex "^(neither)"))


nor : String -> Model -> String
nor string model =
    regexTokenizer string (Regex.caseInsensitive (Regex.regex "^(nor)"))


not : String -> Model -> String
not string model =
    regexTokenizer string (Regex.caseInsensitive (Regex.regex "^(not)"))


and : String -> Model -> String
and string model =
    regexTokenizer string (Regex.caseInsensitive (Regex.regex "^(and)"))


or : String -> Model -> String
or string model =
    regexTokenizer string (Regex.caseInsensitive (Regex.regex "^(or)"))


startQuote : String -> Model -> String
startQuote string model =
    regexTokenizer string (Regex.regex "^(\")")


endQuote : String -> Model -> String
endQuote string model =
    regexTokenizer string (Regex.regex "^(\")")


keyword : String -> Model -> String
keyword string model =
    let
        possibleKeywords =
            List.map .field model.queryFields

        concatenatedKeywordList =
            String.join "|" possibleKeywords

        keywordPattern =
            Regex.caseInsensitive (Regex.regex ("^" ++ model.keywordDelimiter ++ "(" ++ concatenatedKeywordList ++ ")"))
    in
    regexTokenizer string keywordPattern


unknownKeyword : String -> Model -> String
unknownKeyword string model =
    let
        pattern =
            Regex.caseInsensitive (Regex.regex ("^(" ++ model.keywordDelimiter ++ "\\S*)"))
    in
    regexTokenizer string pattern


word : String -> Model -> String
word string model =
    case String.split "" string of
        [] ->
            ""

        first :: rest ->
            case first of
                _ ->
                    let
                        result =
                            regexTokenizer first (Regex.regex "([^ @\",())])")
                    in
                    if result == "" then
                        ""
                    else
                        first ++ word (String.join "" rest) model


space : String -> Model -> String
space string model =
    case String.split "" string of
        [] ->
            ""

        first :: rest ->
            case first of
                " " ->
                    first ++ space (String.join "" rest) model

                _ ->
                    ""


tokenize : String -> (String -> Model -> String) -> Model -> ParsedToken
tokenize string parser model =
    let
        result =
            parser string model

        parsedLength =
            String.length result

        remainingString =
            String.slice parsedLength (String.length string) string
    in
    if parsedLength == 0 then
        ParsedToken "" -1 ""
    else
        ParsedToken result parsedLength remainingString
