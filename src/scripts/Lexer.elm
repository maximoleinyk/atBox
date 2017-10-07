module Lexer exposing (evaluate)

import Dict exposing (Dict)
import Lexeme exposing (Lexeme)
import LexemeType exposing (LexemeType(..))
import LexerState exposing (LexerState(..))
import Model exposing (Model)
import OperatorType exposing (OperatorType(..))
import Token exposing (Token)
import TokenState
    exposing
        ( TokenState
            ( AndTerm
            , CloseParenthesisTerm
            , EitherTerm
            , EndQuoteTerm
            , InTerm
            , IsTerm
            , KeywordTerm
            , NeitherTerm
            , NorTerm
            , NotTerm
            , OpenParenthesisTerm
            , OrTerm
            , SpaceTerm
            , StartQuoteTerm
            , WordTerm
            )
        )


getNextStates : LexerState -> List LexerState
getNextStates state =
    case state of
        START ->
            [ JOIN_TERM, EXPRESSION, START ]

        EXPRESSION ->
            [ OPEN_PARENTHESIS_TERM, OPERATOR_GROUP, CLOSE_PARENTHESIS_TERM ]

        OPERATOR_GROUP ->
            [ FIELD_TERM, OPERATOR_TERM, VALUE_TERM ]

        _ ->
            []


parseCommaSeparatedValues : List Token -> Model -> List Token
parseCommaSeparatedValues tokens model =
    case tokens of
        [] ->
            -- SYNTAX ERROR
            []

        currentToken :: rest ->
            case currentToken.state of
                SpaceTerm ->
                    [ currentToken ] ++ parseCommaSeparatedValues rest model

                OpenParenthesisTerm ->
                    [ currentToken ] ++ parseCommaSeparatedValues rest model

                WordTerm ->
                    [ currentToken ] ++ parseCommaSeparatedValues rest model

                StartQuoteTerm ->
                    [ currentToken ] ++ parseCommaSeparatedValues rest model

                CloseParenthesisTerm ->
                    [ currentToken ]

                _ ->
                    -- SYNTAX ERROR
                    []


parseMultiQuotedWord : List Token -> Model -> List Token
parseMultiQuotedWord tokens model =
    case tokens of
        [] ->
            -- SYNTAX ERROR
            []

        currentToken :: rest ->
            case currentToken.state of
                StartQuoteTerm ->
                    [ currentToken ] ++ parseMultiQuotedWord rest model

                WordTerm ->
                    [ currentToken ] ++ parseMultiQuotedWord rest model

                SpaceTerm ->
                    [ currentToken ] ++ parseMultiQuotedWord rest model

                EndQuoteTerm ->
                    [ currentToken ]

                _ ->
                    -- SYNTAX ERROR
                    []


parseSingleValue : List Token -> Model -> ( List Token, Maybe Lexeme )
parseSingleValue tokens model =
    case tokens of
        [] ->
            -- SYNTAX ERROR
            ( tokens, Nothing )

        first :: rest ->
            case first.state of
                SpaceTerm ->
                    parseSingleValue rest model

                WordTerm ->
                    ( rest, Just (Lexeme Value first.parsedToken.string) )

                StartQuoteTerm ->
                    let
                        result : List Token
                        result =
                            parseMultiQuotedWord tokens model

                        getString =
                            \token -> token.parsedToken.string

                        stringResult =
                            String.join "" (List.map getString result)

                        newTokens =
                            List.drop (List.length result) tokens
                    in
                    if List.length newTokens == List.length tokens then
                        ( tokens, Nothing )
                    else
                        ( newTokens, Just (Lexeme Value stringResult) )

                _ ->
                    ( tokens, Nothing )


parseValueForEitherOrNeitherOperator : List Token -> Model -> String -> ( List Token, Maybe Lexeme )
parseValueForEitherOrNeitherOperator tokens model resultString =
    case tokens of
        [] ->
            -- nothing to parse (syntax error)
            ( tokens, Nothing )

        first :: rest ->
            case first.state of
                SpaceTerm ->
                    parseValueForEitherOrNeitherOperator rest model resultString

                WordTerm ->
                    let
                        ( newTokens, lexeme ) =
                            parseSingleValue tokens model
                    in
                    case lexeme of
                        Nothing ->
                            parseValueForEitherOrNeitherOperator tokens model resultString

                        Just result ->
                            parseValueForEitherOrNeitherOperator newTokens model (resultString ++ result.value)

                StartQuoteTerm ->
                    let
                        ( newTokens, lexeme ) =
                            parseSingleValue tokens model
                    in
                    case lexeme of
                        Nothing ->
                            parseValueForEitherOrNeitherOperator tokens model resultString

                        Just result ->
                            parseValueForEitherOrNeitherOperator newTokens model (resultString ++ result.value)

                OrTerm ->
                    parseValueForEitherOrNeitherOperator rest model (resultString ++ first.parsedToken.string)

                NorTerm ->
                    parseValueForEitherOrNeitherOperator rest model (resultString ++ first.parsedToken.string)

                _ ->
                    if String.length resultString == 0 then
                        -- couldn't parse anything from the beginning only spaces
                        ( tokens, Nothing )
                    else
                        ( tokens, Just (Lexeme Value resultString) )


parseValueForInOperator : List Token -> Model -> ( List Token, Maybe Lexeme )
parseValueForInOperator tokens model =
    if List.length tokens == 0 then
        ( tokens, Nothing )
    else
        case tokens of
            [] ->
                -- syntax error - haven't finished parsing but reached
                ( tokens, Nothing )

            nextToken :: restTokens ->
                case nextToken.state of
                    SpaceTerm ->
                        -- skip any spaces
                        parseValueForInOperator restTokens model

                    OpenParenthesisTerm ->
                        let
                            result : List Token
                            result =
                                parseCommaSeparatedValues tokens model

                            getString =
                                \token -> token.parsedToken.string

                            stringResult =
                                String.join "" (List.map getString result)

                            newTokens =
                                List.drop (List.length result) tokens
                        in
                        if List.length newTokens == List.length tokens then
                            -- couldn't parse any values
                            ( tokens, Nothing )
                        else
                            ( newTokens, Just (Lexeme Value stringResult) )

                    _ ->
                        ( tokens, Nothing )


parseValue : List Token -> Model -> List OperatorType -> ( List Token, Maybe Lexeme )
parseValue tokens model availableOperatorTypes =
    if List.length tokens == 0 then
        ( tokens, Nothing )
    else
        case availableOperatorTypes of
            [] ->
                -- could not parse value for any operator type
                ( tokens, Nothing )

            firstOperatorType :: restOperatorTypes ->
                case firstOperatorType of
                    IsType ->
                        let
                            ( newTokens, lexeme ) =
                                parseSingleValue tokens model
                        in
                        if lexeme == Nothing then
                            parseValue tokens model restOperatorTypes
                        else
                            ( newTokens, lexeme )

                    IsNotType ->
                        let
                            ( newTokens, lexeme ) =
                                parseSingleValue tokens model
                        in
                        if lexeme == Nothing then
                            parseValue tokens model restOperatorTypes
                        else
                            ( newTokens, lexeme )

                    IsEitherType ->
                        let
                            ( newTokens, lexeme ) =
                                parseValueForEitherOrNeitherOperator tokens model ""
                        in
                        case lexeme of
                            Nothing ->
                                parseValue tokens model restOperatorTypes

                            Just result ->
                                ( newTokens, lexeme )

                    IsNeitherType ->
                        let
                            ( newTokens, lexeme ) =
                                parseValueForEitherOrNeitherOperator tokens model ""
                        in
                        case lexeme of
                            Nothing ->
                                parseValue tokens model restOperatorTypes

                            Just result ->
                                ( newTokens, lexeme )

                    IsInType ->
                        let
                            ( newTokens, lexeme ) =
                                parseValueForInOperator tokens model
                        in
                        if lexeme == Nothing then
                            parseValue tokens model restOperatorTypes
                        else
                            ( newTokens, lexeme )

                    IsNotInType ->
                        let
                            ( newTokens, lexeme ) =
                                parseValueForInOperator tokens model
                        in
                        if lexeme == Nothing then
                            parseValue tokens model restOperatorTypes
                        else
                            ( newTokens, lexeme )


parseField : List Token -> Model -> ( List Token, Maybe Lexeme )
parseField tokens model =
    case tokens of
        [] ->
            -- no tokens left - return nothing
            ( tokens, Nothing )

        first :: rest ->
            case first.state of
                KeywordTerm ->
                    -- success - we found a keyword
                    ( rest, Just (Lexeme Field first.parsedToken.string) )

                SpaceTerm ->
                    -- if we encounter space - skip it and take next token
                    parseField rest model

                _ ->
                    -- couldn't parse field
                    ( tokens, Nothing )


parseCloseParenthesis : List Token -> Model -> ( List Token, Maybe Lexeme )
parseCloseParenthesis tokens model =
    case tokens of
        [] ->
            ( tokens, Nothing )

        first :: rest ->
            case first.state of
                CloseParenthesisTerm ->
                    ( rest, Just (Lexeme RightParenthesis first.parsedToken.string) )

                SpaceTerm ->
                    parseCloseParenthesis rest model

                _ ->
                    ( tokens, Nothing )


parseOpenParenthesis : List Token -> Model -> ( List Token, Maybe Lexeme )
parseOpenParenthesis tokens model =
    case tokens of
        [] ->
            ( tokens, Nothing )

        first :: rest ->
            case first.state of
                OpenParenthesisTerm ->
                    ( rest, Just (Lexeme LeftParenthesis first.parsedToken.string) )

                SpaceTerm ->
                    parseOpenParenthesis rest model

                _ ->
                    ( tokens, Nothing )


parseJoin : List Token -> Model -> ( List Token, Maybe Lexeme )
parseJoin tokens model =
    case tokens of
        [] ->
            ( tokens, Nothing )

        first :: rest ->
            case first.state of
                OrTerm ->
                    ( rest, Just (Lexeme Joiner first.parsedToken.string) )

                AndTerm ->
                    ( rest, Just (Lexeme Joiner first.parsedToken.string) )

                SpaceTerm ->
                    parseJoin rest model

                _ ->
                    ( tokens, Nothing )


parseIsSubOperator : List Token -> Model -> String -> ( List Token, Maybe Lexeme )
parseIsSubOperator tokens model resultString =
    if List.length tokens == 0 then
        ( tokens, Nothing )
    else
        case tokens of
            [] ->
                ( tokens, Nothing )

            nextToken :: restTokens ->
                case nextToken.state of
                    SpaceTerm ->
                        parseIsSubOperator restTokens model resultString

                    InTerm ->
                        ( restTokens, Just (Lexeme (Operator IsInType) (resultString ++ nextToken.parsedToken.string)) )

                    EitherTerm ->
                        ( restTokens, Just (Lexeme (Operator IsEitherType) (resultString ++ nextToken.parsedToken.string)) )

                    NeitherTerm ->
                        ( restTokens, Just (Lexeme (Operator IsNeitherType) (resultString ++ nextToken.parsedToken.string)) )

                    NotTerm ->
                        let
                            ( newTokens, lexeme ) =
                                parseIsSubOperator restTokens model resultString
                        in
                        case lexeme of
                            Nothing ->
                                -- there is nothing lower than "is not" there
                                ( restTokens, Just (Lexeme (Operator IsNotType) (resultString ++ nextToken.parsedToken.string)) )

                            Just l ->
                                -- take newTokens here that InTerm returned "is not in" and result value from lexeme
                                ( newTokens, Just (Lexeme (Operator IsNotInType) l.value) )

                    _ ->
                        ( tokens, Nothing )


parseOperator : List Token -> Model -> ( List Token, Maybe Lexeme )
parseOperator tokens model =
    if List.length tokens == 0 then
        ( tokens, Nothing )
    else
        case tokens of
            [] ->
                ( tokens, Nothing )

            nextToken :: restTokens ->
                case nextToken.state of
                    SpaceTerm ->
                        parseOperator restTokens model

                    IsTerm ->
                        let
                            ( newTokens, lexeme ) =
                                parseIsSubOperator tokens model ""
                        in
                        if lexeme == Nothing then
                            -- no sub operators return plain IS
                            ( restTokens, Just (Lexeme (Operator IsType) nextToken.parsedToken.string) )
                        else
                            -- return one of sub operators
                            ( newTokens, lexeme )

                    _ ->
                        -- operator cannot be recognized
                        ( tokens, Nothing )


process : List Token -> Model -> LexerState -> List LexerState -> Dict String Int -> List Lexeme
process tokens model state queue loopDetectionDict =
    let
        tokensLength =
            List.length tokens

        stringifiedState =
            toString state

        newMapping =
            Dict.insert stringifiedState tokensLength loopDetectionDict
    in
    case state of
        JOIN_TERM ->
            let
                ( newTokens, lexeme ) =
                    parseJoin tokens model
            in
            case lexeme of
                Nothing ->
                    walk newTokens model queue newMapping

                Just lexeme ->
                    [ lexeme ] ++ walk newTokens model queue newMapping

        OPEN_PARENTHESIS_TERM ->
            let
                ( newTokens, lexeme ) =
                    parseOpenParenthesis tokens model
            in
            case lexeme of
                Nothing ->
                    walk newTokens model queue newMapping

                Just lexeme ->
                    [ lexeme ] ++ walk newTokens model queue newMapping

        FIELD_TERM ->
            let
                ( newTokens, lexeme ) =
                    parseField tokens model
            in
            case lexeme of
                Nothing ->
                    walk newTokens model queue newMapping

                Just lexeme ->
                    [ lexeme ] ++ walk newTokens model queue newMapping

        OPERATOR_TERM ->
            let
                ( newTokens, lexeme ) =
                    parseOperator tokens model
            in
            case lexeme of
                Nothing ->
                    walk newTokens model queue newMapping

                Just lexeme ->
                    [ lexeme ] ++ walk newTokens model queue newMapping

        VALUE_TERM ->
            let
                ( newTokens, lexeme ) =
                    parseValue tokens model [ IsType, IsNotType, IsEitherType, IsNeitherType, IsInType, IsNotInType ]
            in
            case lexeme of
                Nothing ->
                    walk newTokens model queue newMapping

                Just lexeme ->
                    [ lexeme ] ++ walk newTokens model queue newMapping

        CLOSE_PARENTHESIS_TERM ->
            let
                ( newTokens, lexeme ) =
                    parseOpenParenthesis tokens model
            in
            case lexeme of
                Nothing ->
                    walk newTokens model queue newMapping

                Just lexeme ->
                    [ lexeme ] ++ walk newTokens model queue newMapping

        _ ->
            walk tokens model queue newMapping


walk : List Token -> Model -> List LexerState -> Dict String Int -> List Lexeme
walk tokens model queue loopDetectionDict =
    if List.length tokens == 0 then
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
                        getNextStates state

                    -- prepend states of the current state to the rest in the list
                    newStatesQueue =
                        possibleStates ++ rest

                    -- get previous state of the entry when we were in this state
                    previousLength =
                        Dict.get (toString state) loopDetectionDict

                    currentLength =
                        List.length tokens

                    _ =
                        Debug.log (toString state) newStatesQueue
                in
                case previousLength of
                    Nothing ->
                        -- proceed if record does not exist
                        process tokens model state newStatesQueue loopDetectionDict

                    Just previousLength ->
                        if previousLength == currentLength then
                            -- if both lengths are equal - we are inside infinite loop - try to move to the next state
                            walk tokens model rest loopDetectionDict
                        else
                            -- lengths are different - there is a chance that we might on a correct branch
                            process tokens model state newStatesQueue loopDetectionDict


evaluate : List Token -> Model -> List Lexeme
evaluate tokens model =
    let
        initialState =
            START

        loopDetection =
            Dict.empty
    in
    walk tokens model [ initialState ] loopDetection
