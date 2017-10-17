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


contains : String -> Model -> String
contains string model =
    regexTokenizer string (Regex.caseInsensitive (Regex.regex "^(contains)"))


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


map : String -> Model -> TokenState -> List TokenState -> Dict String String -> TokenState -> Int -> ( List Token, List TokenState )
map string model state queue loopDetectionDict parentState position =
    let
        newMapping =
            Dict.insert (toString state) (toString queue ++ string) loopDetectionDict
    in
    case state of
        CommaTerm ->
            process string model state comma queue newMapping parentState position

        CloseParenthesisInOperatorTerm ->
            process string model state closeParenthesis queue newMapping parentState position

        OpenParenthesisInOperatorTerm ->
            process string model state openParenthesis queue newMapping parentState position

        CloseParenthesisTerm ->
            process string model state closeParenthesis queue newMapping parentState position

        OpenParenthesisTerm ->
            process string model state openParenthesis queue newMapping parentState position

        SpaceTerm ->
            process string model state space queue newMapping parentState position

        WordTerm ->
            process string model state word queue newMapping parentState position

        KeywordTerm ->
            process string model state keyword queue newMapping parentState position

        UnknownKeywordTerm ->
            process string model state unknownKeyword queue newMapping parentState position

        StartQuoteTerm ->
            process string model state startQuote queue newMapping parentState position

        EndQuoteTerm ->
            process string model state endQuote queue newMapping parentState position

        AndTerm ->
            process string model state and queue newMapping parentState position

        OrTerm ->
            process string model state or queue newMapping parentState position

        EitherOrTerm ->
            process string model state or queue newMapping parentState position

        NeitherNorTerm ->
            process string model state nor queue newMapping parentState position

        IsTerm ->
            process string model state is queue newMapping parentState position

        IsNotTerm ->
            process string model state isNot queue newMapping parentState position

        IsInTerm ->
            process string model state isIn queue newMapping parentState position

        IsNotInTerm ->
            process string model state isNotIn queue newMapping parentState position

        IsEitherTerm ->
            process string model state isEither queue newMapping parentState position

        IsNeitherTerm ->
            process string model state isNeither queue newMapping parentState position

        ContainsTerm ->
            process string model state contains queue newMapping parentState position

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
                    previousValue =
                        Dict.get (toString state) loopDetectionDict
                in
                case previousValue of
                    Nothing ->
                        -- proceed if string does not exist
                        map string model state newStatesQueue loopDetectionDict parentState position

                    Just loopDetectionValue ->
                        let
                            queuePlusRemainingString =
                                toString newStatesQueue ++ string
                        in
                        if loopDetectionValue == queuePlusRemainingString then
                            -- if both strings are equal - we are inside infinite loop - try to move further
                            walk string model rest loopDetectionDict state position
                        else
                            -- strings are different - there is a chance that we might on a correct branch
                            map string model state newStatesQueue loopDetectionDict parentState position


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
            [ ContainsOperator, IsEitherOperator, IsNeitherOperator, IsNotInOperator, IsInOperator, IsNotOperator, IsOperator ]

        ContainsOperator ->
            [ ContainsTerm, SpaceTerm, TokenValue ]

        IsOperator ->
            [ IsTerm, SpaceTerm, TokenValue ]

        IsNotOperator ->
            [ IsNotTerm, SpaceTerm, TokenValue ]

        IsInOperator ->
            [ IsInTerm, Statement, OpenParenthesisInOperatorTerm, CommaSeparatedValue, CloseParenthesisInOperatorTerm ]

        IsNotInOperator ->
            [ IsNotInTerm, Statement, OpenParenthesisInOperatorTerm, CommaSeparatedValue, CloseParenthesisInOperatorTerm ]

        IsEitherOperator ->
            [ IsEitherTerm, SpaceTerm, TokenValue, SpaceTerm, EitherOrTerm, SpaceTerm, TokenValue ]

        IsNeitherOperator ->
            [ IsNeitherTerm, SpaceTerm, TokenValue, SpaceTerm, NeitherNorTerm, SpaceTerm, TokenValue ]

        CommaSeparatedValue ->
            [ SpaceTerm, TokenValue, CommaTerm, SpaceTerm, CommaSeparatedValue ]

        TokenValue ->
            [ QuotedWord, Word ]

        Word ->
            [ WordTerm ]

        QuotedWord ->
            [ StartQuoteTerm, Statement, EndQuoteTerm ]

        Conjunction ->
            [ OrConjunction, AndConjunction ]

        OrConjunction ->
            [ OrTerm ]

        AndConjunction ->
            [ AndTerm ]

        _ ->
            []


processFailedResult : String -> Model -> TokenState -> List TokenState -> Dict String String -> TokenState -> Int -> ( List Token, List TokenState )
processFailedResult string model state queue loopDetectionDict parentState newPosition =
    let
        dropNextStatesAndWalk =
            \n ->
                walk string model (List.drop n queue) loopDetectionDict parentState newPosition
    in
    case state of
        ContainsTerm ->
            dropNextStatesAndWalk 2

        OpenParenthesisInOperatorTerm ->
            dropNextStatesAndWalk 2

        KeywordTerm ->
            walk string model (UnknownKeywordTerm :: queue) loopDetectionDict parentState newPosition

        StartQuoteTerm ->
            dropNextStatesAndWalk 2

        IsTerm ->
            walk string model (TokenValue :: List.drop 2 queue) loopDetectionDict parentState newPosition

        IsNotTerm ->
            dropNextStatesAndWalk 2

        IsInTerm ->
            dropNextStatesAndWalk 4

        IsNotInTerm ->
            dropNextStatesAndWalk 4

        IsEitherTerm ->
            dropNextStatesAndWalk 6

        IsNeitherTerm ->
            dropNextStatesAndWalk 6

        _ ->
            walk string model queue loopDetectionDict parentState newPosition


process : String -> Model -> TokenState -> (String -> Model -> String) -> List TokenState -> Dict String String -> TokenState -> Int -> ( List Token, List TokenState )
process string model state tokenizer queue loopDetectionDict parentState position =
    let
        result =
            tokenizer string model
    in
    if result == "" then
        processFailedResult string model state queue loopDetectionDict parentState position
    else
        processSuccessfulResult result string model state queue loopDetectionDict parentState position


processSuccessfulResult : String -> String -> Model -> TokenState -> List TokenState -> Dict String String -> TokenState -> Int -> ( List Token, List TokenState )
processSuccessfulResult result string model state queue loopDetectionDict parentState position =
    let
        resultLength =
            String.length result

        newString =
            String.slice resultLength (String.length string) string

        newPosition =
            position + resultLength

        token =
            [ Token state result position ]

        dropAheadAndWalk =
            \n ->
                let
                    possibleStates =
                        List.drop 1 (getPossibleStates parentState)

                    numberOfItemsToDrop =
                        List.length possibleStates + n

                    updatedQueue =
                        possibleStates ++ List.drop numberOfItemsToDrop queue

                    ( result, remainingStates ) =
                        walk newString model updatedQueue loopDetectionDict parentState newPosition
                in
                ( token ++ result, remainingStates )
    in
    case state of
        ContainsTerm ->
            dropAheadAndWalk 6

        OrTerm ->
            dropAheadAndWalk 1

        StartQuoteTerm ->
            dropAheadAndWalk 1

        IsEitherTerm ->
            dropAheadAndWalk 5

        IsNeitherTerm ->
            dropAheadAndWalk 4

        IsNotInTerm ->
            dropAheadAndWalk 3

        IsInTerm ->
            dropAheadAndWalk 2

        IsNotTerm ->
            dropAheadAndWalk 1

        _ ->
            let
                ( result, remainingStates ) =
                    walk newString model queue loopDetectionDict parentState newPosition
            in
            ( token ++ result, remainingStates )
