module Tokenizer exposing (ParsedToken, Token, TokenState(..), run)

import Dict exposing (Dict)
import Model exposing (Model)
import Regex exposing (HowMany(All), Regex)


type alias Token =
    { state : TokenState
    , parsedToken : ParsedToken
    }


type TokenState
    = Start
    | Statement
    | SpaceTerm
    | WordTerm
    | KeywordTerm
    | Value
    | MultiQuotedWord
    | StartQuoteTerm
    | EndQuoteTerm
    | Criteria
    | Criterion
    | Conjunction
    | Operator
    | OperatorGroup
    | EitherOrTerm
    | AndTerm
    | OrTerm
    | NorTerm
    | NotTerm
    | IsTerm
    | IsOperator
    | IsSubOperator
    | EitherTerm
    | EitherOrOperator
    | NeitherTerm
    | NeitherNorOperator
    | UnknownKeywordTerm
    | InTerm
    | InOperator
    | InValue
    | OpenParenthesisTerm
    | CloseParenthesisTerm
    | CommaTerm
    | InRepeatValue


type alias ParsedToken =
    { string : String
    , length : Int
    }


run : String -> Model -> List Token
run string model =
    let
        initialState =
            Start

        loopDetection =
            Dict.empty
    in
    walk string model [ initialState ] loopDetection initialState


regexTokenizer : String -> Regex -> String
regexTokenizer string pattern =
    let
        patternMatches =
            Regex.find All pattern string

        result =
            List.head (List.map .match patternMatches)
    in
    case result of
        Just string ->
            string

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
tokenize string tokenizer model =
    let
        result =
            tokenizer string model

        resultLength =
            String.length result
    in
    if resultLength == 0 then
        ParsedToken "" -1
    else
        ParsedToken result resultLength


return : String -> Model -> TokenState -> (String -> Model -> String) -> List TokenState -> Dict String String -> TokenState -> List Token
return string model state tokenizer queue newMapping parentState =
    let
        result =
            tokenize string tokenizer model

        newQueue =
            \n q -> List.drop n q
    in
    if result.length == -1 then
        case state of
            OpenParenthesisTerm ->
                let
                    numberOfStatesToDrop =
                        if parentState == Criterion then
                            1
                        else
                            2
                in
                walk string model (newQueue numberOfStatesToDrop queue) newMapping parentState

            KeywordTerm ->
                walk string model (UnknownKeywordTerm :: queue) newMapping parentState

            StartQuoteTerm ->
                walk string model (newQueue 2 queue) newMapping parentState

            EitherTerm ->
                walk string model (newQueue 6 queue) newMapping parentState

            NeitherTerm ->
                walk string model (newQueue 6 queue) newMapping parentState

            _ ->
                walk string model queue newMapping parentState
    else
        let
            newString =
                String.slice result.length (String.length string) string

            token =
                [ Token state result ]
        in
        case state of
            OrTerm ->
                token ++ walk newString model (newQueue 3 queue) newMapping parentState

            AndTerm ->
                token ++ walk newString model (newQueue 2 queue) newMapping parentState

            _ ->
                token ++ walk newString model queue newMapping parentState


process : String -> Model -> TokenState -> List TokenState -> Dict String String -> TokenState -> List Token
process string model state queue loopDetectionDict parentState =
    let
        -- string neither exists or not equals in the loopDetectionDict
        newMapping =
            Dict.insert (toString state) string loopDetectionDict
    in
    case state of
        CommaTerm ->
            return string model state comma queue newMapping parentState

        CloseParenthesisTerm ->
            return string model state closeParenthesis queue newMapping parentState

        OpenParenthesisTerm ->
            return string model state openParenthesis queue newMapping parentState

        InTerm ->
            return string model state inTerm queue newMapping parentState

        SpaceTerm ->
            return string model state space queue newMapping parentState

        WordTerm ->
            return string model state word queue newMapping parentState

        KeywordTerm ->
            return string model state keyword queue newMapping parentState

        UnknownKeywordTerm ->
            return string model state unknownKeyword queue newMapping parentState

        StartQuoteTerm ->
            return string model state startQuote queue newMapping parentState

        EndQuoteTerm ->
            return string model state endQuote queue newMapping parentState

        AndTerm ->
            return string model state and queue newMapping parentState

        OrTerm ->
            return string model state or queue newMapping parentState

        EitherOrTerm ->
            return string model state or queue newMapping parentState

        NorTerm ->
            return string model state nor queue newMapping parentState

        NotTerm ->
            return string model state not queue newMapping parentState

        IsTerm ->
            return string model state is queue newMapping parentState

        EitherTerm ->
            return string model state either queue newMapping parentState

        NeitherTerm ->
            return string model state neither queue newMapping parentState

        _ ->
            walk string model queue newMapping state


walk : String -> Model -> List TokenState -> Dict String String -> TokenState -> List Token
walk string model queue loopDetectionDict parentState =
    -- empty string means we finished parsing
    if string == "" then
        []
    else
        -- assess queue of upcoming states
        case queue of
            -- is queue is empty it means we finished parsing
            [] ->
                []

            -- process first state
            state :: rest ->
                let
                    -- get list of next states
                    possibleStates =
                        getPossibleStates state

                    -- prepend states of the current state to the rest
                    newStatesQueue =
                        possibleStates ++ rest

                    -- get previous state of the entry when we were in this state
                    previousString =
                        Dict.get (toString state) loopDetectionDict

                    --                    _ =
                    --                        Debug.log (toString state) newStatesQueue
                in
                case previousString of
                    Nothing ->
                        -- proceed if string does not exist
                        process string model state newStatesQueue loopDetectionDict parentState

                    Just previousString ->
                        if previousString == string then
                            -- if both strings are equal - we are inside infinite loop - try to move to the next state
                            walk string model rest loopDetectionDict state
                        else
                            -- strings are different - there is a chance that we might on a correct branch
                            process string model state newStatesQueue loopDetectionDict parentState


getPossibleStates : TokenState -> List TokenState
getPossibleStates state =
    case state of
        Start ->
            [ Statement, Criteria ]

        Statement ->
            [ SpaceTerm, WordTerm, Statement ]

        Criteria ->
            [ Criterion, Start ]

        Criterion ->
            [ OpenParenthesisTerm, SpaceTerm, OperatorGroup, SpaceTerm, CloseParenthesisTerm, SpaceTerm, Conjunction ]

        OperatorGroup ->
            [ KeywordTerm, SpaceTerm, Operator, SpaceTerm, Value ]

        Operator ->
            [ IsOperator ]

        IsOperator ->
            [ IsTerm, SpaceTerm, IsSubOperator ]

        IsSubOperator ->
            [ EitherOrOperator, NeitherNorOperator, NotTerm, SpaceTerm, InTerm ]

        Value ->
            [ WordTerm, MultiQuotedWord, InValue ]

        InValue ->
            [ SpaceTerm, OpenParenthesisTerm, InRepeatValue, CloseParenthesisTerm ]

        InRepeatValue ->
            [ SpaceTerm, CommaTerm, Value, InRepeatValue ]

        MultiQuotedWord ->
            [ StartQuoteTerm, Statement, EndQuoteTerm ]

        EitherOrOperator ->
            [ SpaceTerm, EitherTerm, SpaceTerm, Value, SpaceTerm, EitherOrTerm, SpaceTerm, EitherOrOperator ]

        NeitherNorOperator ->
            [ SpaceTerm, NeitherTerm, SpaceTerm, Value, SpaceTerm, NorTerm, SpaceTerm, NeitherNorOperator ]

        Conjunction ->
            [ SpaceTerm, OrTerm, AndTerm, WordTerm, Conjunction ]

        _ ->
            []
