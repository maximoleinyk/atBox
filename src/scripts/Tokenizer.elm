module Tokenizer exposing (getPossibleStates, isTermState, run)

import Dict exposing (Dict)
import GlobalTypes exposing (Model, Token, TokenState(..))
import Regex exposing (HowMany(All), Regex)


run : String -> Model -> ( List Token, List TokenState )
run string model =
    let
        initialState =
            Start

        loopDetection =
            Dict.empty
    in
    walk string model [ initialState ] loopDetection initialState 0


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


isIn : String -> Model -> String
isIn string model =
    regexTokenizer string (Regex.caseInsensitive (Regex.regex "^(is\\s+in)"))


isNotIn : String -> Model -> String
isNotIn string model =
    regexTokenizer string (Regex.caseInsensitive (Regex.regex "^(is\\s+not\\s+in)"))


is : String -> Model -> String
is string model =
    regexTokenizer string (Regex.caseInsensitive (Regex.regex "^(is)"))


isNot : String -> Model -> String
isNot string model =
    regexTokenizer string (Regex.caseInsensitive (Regex.regex "^(is\\s+not)"))


isEither : String -> Model -> String
isEither string model =
    regexTokenizer string (Regex.caseInsensitive (Regex.regex "^(is\\s+either)"))


isNeither : String -> Model -> String
isNeither string model =
    regexTokenizer string (Regex.caseInsensitive (Regex.regex "^(is\\s+neither)"))


comma : String -> Model -> String
comma string model =
    regexTokenizer string (Regex.regex "^(,)")


nor : String -> Model -> String
nor string model =
    regexTokenizer string (Regex.caseInsensitive (Regex.regex "^(nor)"))


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
            "^(" ++ model.keywordDelimiter ++ "\\S*)"

        regexp =
            Regex.caseInsensitive (Regex.regex pattern)
    in
    regexTokenizer string regexp


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
                            regexTokenizer first (Regex.regex ("([^ " ++ model.keywordDelimiter ++ "\",())])"))
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


process : String -> Model -> TokenState -> List TokenState -> Dict String String -> TokenState -> Int -> ( List Token, List TokenState )
process string model state queue loopDetectionDict parentState position =
    let
        -- string neither exists or not equals in the loopDetectionDict
        newMapping =
            Dict.insert (toString state) string loopDetectionDict
    in
    case state of
        CommaTerm ->
            return string model state comma queue newMapping parentState position

        CloseParenthesisInOperatorTerm ->
            return string model state closeParenthesis queue newMapping parentState position

        OpenParenthesisInOperatorTerm ->
            return string model state openParenthesis queue newMapping parentState position

        CloseParenthesisTerm ->
            return string model state closeParenthesis queue newMapping parentState position

        OpenParenthesisTerm ->
            return string model state openParenthesis queue newMapping parentState position

        SpaceTerm ->
            return string model state space queue newMapping parentState position

        WordTerm ->
            return string model state word queue newMapping parentState position

        KeywordTerm ->
            return string model state keyword queue newMapping parentState position

        UnknownKeywordTerm ->
            return string model state unknownKeyword queue newMapping parentState position

        StartQuoteTerm ->
            return string model state startQuote queue newMapping parentState position

        EndQuoteTerm ->
            return string model state endQuote queue newMapping parentState position

        AndTerm ->
            return string model state and queue newMapping parentState position

        OrTerm ->
            return string model state or queue newMapping parentState position

        EitherOrTerm ->
            return string model state or queue newMapping parentState position

        NeitherNorTerm ->
            return string model state nor queue newMapping parentState position

        IsTerm ->
            return string model state is queue newMapping parentState position

        IsNotTerm ->
            return string model state isNot queue newMapping parentState position

        IsInTerm ->
            return string model state isIn queue newMapping parentState position

        IsNotInTerm ->
            return string model state isNotIn queue newMapping parentState position

        IsEitherTerm ->
            return string model state isEither queue newMapping parentState position

        IsNeitherTerm ->
            return string model state isNeither queue newMapping parentState position

        _ ->
            walk string model queue newMapping state position


walk : String -> Model -> List TokenState -> Dict String String -> TokenState -> Int -> ( List Token, List TokenState )
walk string model queue loopDetectionDict parentState position =
    -- empty string means we finished parsing
    if string == "" then
        ( [], queue )
    else
        -- assess queue of upcoming states
        case queue of
            -- is queue is empty it means we finished parsing
            [] ->
                ( [], queue )

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
                in
                case previousString of
                    Nothing ->
                        -- proceed if string does not exist
                        process string model state newStatesQueue loopDetectionDict parentState position

                    Just previousString ->
                        if previousString == string then
                            -- if both strings are equal - we are inside infinite loop - try to move to the next state
                            walk string model rest loopDetectionDict state position
                        else
                            -- strings are different - there is a chance that we might on a correct branch
                            process string model state newStatesQueue loopDetectionDict parentState position


isTermState : TokenState -> Bool
isTermState state =
    List.length (getPossibleStates state) == 0


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
            [ KeywordTerm, SpaceTerm, TokenOperator ]

        TokenOperator ->
            [ IsEitherOperator, IsNeitherOperator, IsNotInOperator, IsInOperator, IsNotOperator, IsOperator ]

        IsOperator ->
            [ IsTerm, SpaceTerm, TokenValue ]

        IsNotOperator ->
            [ IsNotTerm, SpaceTerm, TokenValue ]

        IsInOperator ->
            [ IsInTerm, Statement, OpenParenthesisInOperatorTerm, InValue, CloseParenthesisInOperatorTerm ]

        IsNotInOperator ->
            [ IsNotInTerm, Statement, OpenParenthesisInOperatorTerm, InValue, CloseParenthesisInOperatorTerm ]

        IsEitherOperator ->
            [ IsEitherTerm, SpaceTerm, TokenValue, SpaceTerm, EitherOrTerm, SpaceTerm, TokenValue ]

        IsNeitherOperator ->
            [ IsNeitherTerm, SpaceTerm, TokenValue, SpaceTerm, NeitherNorTerm, SpaceTerm, TokenValue ]

        InValue ->
            [ SpaceTerm, TokenValue, CommaTerm, SpaceTerm, InValue ]

        TokenValue ->
            [ SingleWord, MultiQuotedWord ]

        SingleWord ->
            [ WordTerm ]

        MultiQuotedWord ->
            [ StartQuoteTerm, Statement, EndQuoteTerm ]

        Conjunction ->
            [ OrConjunction, AndConjunction ]

        OrConjunction ->
            [ OrTerm ]

        AndConjunction ->
            [ AndTerm ]

        _ ->
            []


return : String -> Model -> TokenState -> (String -> Model -> String) -> List TokenState -> Dict String String -> TokenState -> Int -> ( List Token, List TokenState )
return string model state tokenizer queue newMapping parentState position =
    let
        result =
            tokenizer string model

        resultLength =
            String.length result

        newPosition =
            position + resultLength

        newQueue =
            \n q -> List.drop n q
    in
    if result == "" then
        case state of
            OpenParenthesisInOperatorTerm ->
                walk string model (newQueue 2 queue) newMapping parentState newPosition

            KeywordTerm ->
                walk string model (UnknownKeywordTerm :: queue) newMapping parentState newPosition

            StartQuoteTerm ->
                walk string model (newQueue 2 queue) newMapping parentState newPosition

            IsTerm ->
                walk string model (newQueue 2 queue) newMapping parentState newPosition

            IsNotTerm ->
                walk string model (newQueue 2 queue) newMapping parentState newPosition

            IsInTerm ->
                walk string model (newQueue 4 queue) newMapping parentState newPosition

            IsNotInTerm ->
                walk string model (newQueue 4 queue) newMapping parentState newPosition

            IsEitherTerm ->
                walk string model (newQueue 6 queue) newMapping parentState newPosition

            IsNeitherTerm ->
                walk string model (newQueue 6 queue) newMapping parentState newPosition

            _ ->
                walk string model queue newMapping parentState newPosition
    else
        let
            newString =
                String.slice resultLength (String.length string) string

            token =
                [ Token state result position ]

            dropAhead n q =
                let
                    possibleStates =
                        List.drop 1 (getPossibleStates parentState)

                    numberOfItemsToDrop =
                        List.length possibleStates + n
                in
                possibleStates ++ List.drop numberOfItemsToDrop q
        in
        case state of
            OrTerm ->
                let
                    ( result, remainingStates ) =
                        walk newString model (newQueue 1 queue) newMapping parentState newPosition
                in
                ( token ++ result, remainingStates )

            WordTerm ->
                let
                    ( result, remainingStates ) =
                        walk newString model (newQueue 1 queue) newMapping parentState newPosition
                in
                ( token ++ result, remainingStates )

            IsEitherTerm ->
                let
                    ( result, remainingStates ) =
                        walk newString model (dropAhead 5 queue) newMapping parentState newPosition
                in
                ( token ++ result, remainingStates )

            IsNeitherTerm ->
                let
                    ( result, remainingStates ) =
                        walk newString model (dropAhead 4 queue) newMapping parentState newPosition
                in
                ( token ++ result, remainingStates )

            IsNotInTerm ->
                let
                    ( result, remainingStates ) =
                        walk newString model (dropAhead 3 queue) newMapping parentState newPosition
                in
                ( token ++ result, remainingStates )

            IsInTerm ->
                let
                    ( result, remainingStates ) =
                        walk newString model (dropAhead 2 queue) newMapping parentState newPosition
                in
                ( token ++ result, remainingStates )

            IsNotTerm ->
                let
                    ( result, remainingStates ) =
                        walk newString model (dropAhead 1 queue) newMapping parentState newPosition
                in
                ( token ++ result, remainingStates )

            _ ->
                let
                    ( result, remainingStates ) =
                        walk newString model queue newMapping parentState newPosition
                in
                ( token ++ result, remainingStates )
