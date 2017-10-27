module Lexer exposing (run)

import Dict exposing (Dict)
import GlobalTypes exposing (Lexeme, LexemeState(Field, Joiner, LeftParenthesis, LexemeValue, Operator, RightParenthesis, UnknownField), LexerState(..), Model, OperatorType(ContainsType, IsEitherType, IsInType, IsNeitherType, IsNotInType, IsNotType, IsType), Token, TokenState(..))
import Regex


getNextStates : LexerState -> List LexerState
getNextStates state =
    case state of
        START ->
            [ JOIN_TERM, EXPRESSION, START ]

        EXPRESSION ->
            [ OPEN_PARENTHESIS, OPERATOR_GROUP, CLOSE_PARENTHESIS ]

        OPEN_PARENTHESIS ->
            [ OPEN_PARENTHESIS_TERM, OPEN_PARENTHESIS ]

        CLOSE_PARENTHESIS ->
            [ CLOSE_PARENTHESIS_TERM, CLOSE_PARENTHESIS ]

        OPERATOR_GROUP ->
            [ FIELD_TERM, OPERATOR_TERM, VALUE_TERM ]

        _ ->
            []


parseCommaSeparatedValues : List Token -> Model -> List Token
parseCommaSeparatedValues tokens model =
    case tokens of
        [] ->
            []

        currentToken :: rest ->
            case currentToken.state of
                CloseParenthesisInOperatorTerm ->
                    [ currentToken ]

                _ ->
                    [ currentToken ] ++ parseCommaSeparatedValues rest model


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


parseSingleValue : List Token -> Model -> Int -> Int -> ( List Token, Maybe Lexeme, Int )
parseSingleValue tokens model initialPosition endValuePosition =
    let
        nothing =
            ( tokens, Nothing, endValuePosition )
    in
    case tokens of
        [] ->
            nothing

        first :: rest ->
            let
                newPosition =
                    endValuePosition + String.length first.value
            in
            case first.state of
                SpaceTerm ->
                    parseSingleValue rest model newPosition newPosition

                WordTerm ->
                    ( rest, Just (Lexeme LexemeValue first.value initialPosition), newPosition )

                StartQuoteTerm ->
                    let
                        result : List Token
                        result =
                            parseMultiQuotedWord tokens model

                        getString =
                            \token -> token.value

                        stringResult =
                            String.join "" (List.map getString result)

                        newTokens =
                            List.drop (List.length result) tokens

                        -- including quotes "  some   value "
                        newPosition =
                            endValuePosition + String.length stringResult
                    in
                    if List.length newTokens == List.length tokens then
                        nothing
                    else
                        ( newTokens, Just (Lexeme LexemeValue stringResult initialPosition), newPosition )

                _ ->
                    nothing


parseValueForEitherOrNeitherOperator : List Token -> Model -> String -> Int -> Int -> ( List Token, Maybe Lexeme, Int )
parseValueForEitherOrNeitherOperator tokens model resultString initialPosition endValuePosition =
    let
        returnResult =
            if String.length resultString == 0 then
                ( tokens, Nothing, initialPosition )
            else
                ( tokens, Just (Lexeme LexemeValue resultString initialPosition), endValuePosition )
    in
    case tokens of
        [] ->
            returnResult

        first :: rest ->
            let
                newPosition =
                    endValuePosition + String.length first.value
            in
            case first.state of
                SpaceTerm ->
                    parseValueForEitherOrNeitherOperator rest model (resultString ++ first.value) initialPosition newPosition

                WordTerm ->
                    let
                        ( newTokens, lexeme, newPosition ) =
                            parseSingleValue tokens model initialPosition endValuePosition
                    in
                    case lexeme of
                        Nothing ->
                            parseValueForEitherOrNeitherOperator tokens model resultString initialPosition newPosition

                        Just result ->
                            parseValueForEitherOrNeitherOperator newTokens model (resultString ++ result.value) initialPosition newPosition

                StartQuoteTerm ->
                    let
                        ( newTokens, lexeme, newPosition ) =
                            parseSingleValue tokens model initialPosition initialPosition
                    in
                    case lexeme of
                        Nothing ->
                            parseValueForEitherOrNeitherOperator tokens model resultString initialPosition newPosition

                        Just result ->
                            parseValueForEitherOrNeitherOperator newTokens model (resultString ++ result.value) initialPosition newPosition

                EitherOrTerm ->
                    let
                        newPosition =
                            endValuePosition + String.length first.value
                    in
                    parseValueForEitherOrNeitherOperator rest model (resultString ++ first.value) initialPosition newPosition

                NeitherNorTerm ->
                    let
                        newPosition =
                            endValuePosition + String.length first.value
                    in
                    parseValueForEitherOrNeitherOperator rest model (resultString ++ first.value) initialPosition newPosition

                _ ->
                    returnResult


parseValueForInOperator : List Token -> Model -> Int -> Int -> ( List Token, Maybe Lexeme, Int )
parseValueForInOperator tokens model initialPosition endValuePosition =
    case tokens of
        [] ->
            ( tokens, Just (Lexeme LexemeValue "" initialPosition), endValuePosition )

        nextToken :: restTokens ->
            case nextToken.state of
                SpaceTerm ->
                    let
                        newPosition =
                            endValuePosition + String.length nextToken.value
                    in
                    parseValueForInOperator restTokens model initialPosition newPosition

                WordTerm ->
                    let
                        newPosition =
                            endValuePosition + String.length nextToken.value
                    in
                    parseValueForInOperator restTokens model initialPosition newPosition

                OpenParenthesisInOperatorTerm ->
                    let
                        result : List Token
                        result =
                            parseCommaSeparatedValues tokens model

                        getString =
                            \token -> token.value

                        stringResult =
                            String.join "" (List.map getString result)

                        newTokens =
                            List.drop (List.length result) tokens

                        newPosition =
                            endValuePosition + String.length stringResult
                    in
                    if List.length newTokens == List.length tokens then
                        -- syntax error
                        ( tokens, Nothing, initialPosition )
                    else
                        ( newTokens, Just (Lexeme LexemeValue stringResult initialPosition), newPosition )

                _ ->
                    -- parser cannot recognize input
                    ( tokens, Nothing, initialPosition )


parseValue : List Token -> Model -> Maybe Lexeme -> Int -> ( List Token, Maybe Lexeme, Int )
parseValue tokens model previousLexeme position =
    let
        nothing =
            ( tokens, Nothing, position )
    in
    if List.length tokens == 0 then
        nothing
    else
        case previousLexeme of
            Nothing ->
                nothing

            Just previousLexeme ->
                let
                    state =
                        previousLexeme.state
                in
                case state of
                    Operator operatorType ->
                        -- order is important here cannot use "case of" operator
                        if operatorType == IsEitherType || operatorType == IsNeitherType then
                            let
                                ( newTokens, lexeme, newPosition ) =
                                    parseValueForEitherOrNeitherOperator tokens model "" position position
                            in
                            case lexeme of
                                Nothing ->
                                    nothing

                                Just result ->
                                    ( newTokens, lexeme, newPosition )
                        else if operatorType == IsInType || operatorType == IsNotInType then
                            let
                                ( newTokens, lexeme, newPosition ) =
                                    parseValueForInOperator tokens model position position
                            in
                            if lexeme == Nothing then
                                nothing
                            else
                                ( newTokens, lexeme, newPosition )
                        else if operatorType == IsType || operatorType == IsNotType || operatorType == ContainsType then
                            let
                                ( newTokens, lexeme, newPosition ) =
                                    parseSingleValue tokens model position position
                            in
                            if lexeme == Nothing then
                                nothing
                            else
                                ( newTokens, lexeme, newPosition )
                        else
                            nothing

                    _ ->
                        nothing


parseField : List Token -> Model -> Int -> ( List Token, Maybe Lexeme, Int )
parseField tokens model position =
    case tokens of
        [] ->
            -- no tokens left - return nothing
            ( tokens, Nothing, position )

        first :: rest ->
            let
                newPosition =
                    position + String.length first.value
            in
            case first.state of
                KeywordTerm ->
                    -- success - we found a keyword
                    ( rest, Just (Lexeme Field first.value position), newPosition )

                UnknownKeywordTerm ->
                    ( rest, Just (Lexeme UnknownField first.value position), newPosition )

                SpaceTerm ->
                    -- if we encounter space - skip it and take next token
                    parseField rest model newPosition

                WordTerm ->
                    parseField rest model newPosition

                _ ->
                    -- couldn't parse field
                    ( tokens, Nothing, newPosition )


parseCloseParenthesis : List Token -> Model -> Int -> ( List Token, Maybe Lexeme, Int )
parseCloseParenthesis tokens model position =
    case tokens of
        [] ->
            ( tokens, Nothing, position )

        first :: rest ->
            let
                newPosition =
                    position + String.length first.value
            in
            case first.state of
                SpaceTerm ->
                    parseCloseParenthesis rest model newPosition

                CloseParenthesisTerm ->
                    ( rest, Just (Lexeme RightParenthesis first.value position), newPosition )

                _ ->
                    ( tokens, Nothing, position )


parseOpenParenthesis : List Token -> Model -> Int -> ( List Token, Maybe Lexeme, Int )
parseOpenParenthesis tokens model position =
    case tokens of
        [] ->
            ( tokens, Nothing, position )

        first :: rest ->
            let
                newPosition =
                    position + String.length first.value
            in
            case first.state of
                SpaceTerm ->
                    parseOpenParenthesis rest model newPosition

                OpenParenthesisTerm ->
                    ( rest, Just (Lexeme LeftParenthesis first.value position), newPosition )

                _ ->
                    ( tokens, Nothing, position )


parseJoin : List Token -> Model -> Int -> ( List Token, Maybe Lexeme, Int )
parseJoin tokens model position =
    case tokens of
        [] ->
            ( tokens, Nothing, position )

        first :: rest ->
            let
                newPosition =
                    position + String.length first.value
            in
            case first.state of
                OrTerm ->
                    ( rest, Just (Lexeme Joiner first.value position), newPosition )

                AndTerm ->
                    ( rest, Just (Lexeme Joiner first.value position), newPosition )

                WordTerm ->
                    parseJoin rest model newPosition

                SpaceTerm ->
                    parseJoin rest model newPosition

                _ ->
                    ( tokens, Nothing, position )


parseOperator : List Token -> Model -> Int -> Int -> ( List Token, Maybe Lexeme, Int )
parseOperator tokens model initPosition endOperatorPosition =
    let
        nothing =
            ( tokens, Nothing, initPosition )
    in
    case tokens of
        [] ->
            nothing

        nextToken :: restTokens ->
            let
                newPosition =
                    endOperatorPosition + String.length nextToken.value
            in
            case nextToken.state of
                SpaceTerm ->
                    parseOperator restTokens model newPosition newPosition

                ContainsTerm ->
                    ( restTokens, Just (Lexeme (Operator ContainsType) nextToken.value initPosition), newPosition )

                IsTerm ->
                    ( restTokens, Just (Lexeme (Operator IsType) nextToken.value initPosition), newPosition )

                IsNotTerm ->
                    ( restTokens, Just (Lexeme (Operator IsNotType) nextToken.value initPosition), newPosition )

                IsInTerm ->
                    ( restTokens, Just (Lexeme (Operator IsInType) nextToken.value initPosition), newPosition )

                IsNotInTerm ->
                    ( restTokens, Just (Lexeme (Operator IsNotInType) nextToken.value initPosition), newPosition )

                IsEitherTerm ->
                    ( restTokens, Just (Lexeme (Operator IsEitherType) nextToken.value initPosition), newPosition )

                IsNeitherTerm ->
                    ( restTokens, Just (Lexeme (Operator IsNeitherType) nextToken.value initPosition), newPosition )

                _ ->
                    -- unreachable expression
                    nothing


process : List Token -> Model -> LexerState -> List LexerState -> Dict String Int -> Maybe Lexeme -> Int -> List Lexeme
process tokens model state queue loopDetectionDict previousLexeme position =
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
                ( newTokens, lexeme, newPosition ) =
                    parseJoin tokens model position
            in
            case lexeme of
                Nothing ->
                    traverse newTokens model queue newMapping previousLexeme newPosition

                Just lexeme ->
                    [ lexeme ] ++ traverse newTokens model queue newMapping (Just lexeme) newPosition

        OPEN_PARENTHESIS_TERM ->
            let
                ( newTokens, lexeme, newPosition ) =
                    parseOpenParenthesis tokens model position
            in
            case lexeme of
                Nothing ->
                    let
                        -- if '(' cannot be read remove EXPRESSION state
                        newQueue =
                            List.drop 1 queue
                    in
                    traverse newTokens model newQueue newMapping previousLexeme newPosition

                Just lexeme ->
                    [ lexeme ] ++ traverse newTokens model queue newMapping (Just lexeme) newPosition

        FIELD_TERM ->
            let
                ( newTokens, lexeme, newPosition ) =
                    parseField tokens model position
            in
            case lexeme of
                Nothing ->
                    let
                        {-
                           if we couldn't parse FIELD term there is no point of parsing
                           next OPERATOR and VALUE terms that come after
                        -}
                        newQueue =
                            List.drop 2 queue
                    in
                    traverse newTokens model newQueue newMapping previousLexeme newPosition

                Just lexeme ->
                    [ lexeme ] ++ traverse newTokens model queue newMapping (Just lexeme) newPosition

        OPERATOR_TERM ->
            let
                ( newTokens, lexeme, newPosition ) =
                    parseOperator tokens model position position
            in
            case lexeme of
                Nothing ->
                    let
                        {-
                           if we couldn't parse FIELD term there is no point of parsing
                           next OPERATOR and VALUE terms that come after
                        -}
                        newQueue =
                            List.drop 1 queue
                    in
                    traverse newTokens model newQueue newMapping previousLexeme newPosition

                Just lexeme ->
                    [ lexeme ] ++ traverse newTokens model queue newMapping (Just lexeme) newPosition

        VALUE_TERM ->
            let
                ( newTokens, lexeme, newPosition ) =
                    parseValue tokens model previousLexeme position
            in
            case lexeme of
                Nothing ->
                    traverse newTokens model queue newMapping previousLexeme position

                Just lexeme ->
                    [ lexeme ] ++ traverse newTokens model queue newMapping (Just lexeme) newPosition

        CLOSE_PARENTHESIS_TERM ->
            let
                ( newTokens, lexeme, newPosition ) =
                    parseCloseParenthesis tokens model position
            in
            case lexeme of
                Nothing ->
                    let
                        -- if ')' cannot be read remove EXPRESSION state in order to avoid infinite loop
                        newQueue =
                            List.drop 1 queue
                    in
                    traverse newTokens model newQueue newMapping previousLexeme newPosition

                Just lexeme ->
                    [ lexeme ] ++ traverse newTokens model queue newMapping (Just lexeme) newPosition

        _ ->
            traverse tokens model queue newMapping previousLexeme position


traverse : List Token -> Model -> List LexerState -> Dict String Int -> Maybe Lexeme -> Int -> List Lexeme
traverse tokens model queue loopDetectionDict previousLexeme position =
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
                in
                case previousLength of
                    Nothing ->
                        -- proceed if record does not exist
                        process tokens model state newStatesQueue loopDetectionDict previousLexeme position

                    Just previousLength ->
                        if previousLength == currentLength then
                            -- if both lengths are equal - we are inside infinite loop - try to move to the next state
                            traverse tokens model rest loopDetectionDict previousLexeme position
                        else
                            -- lengths are different - there is a chance that we might on a correct branch
                            process tokens model state newStatesQueue loopDetectionDict previousLexeme position


run : List Token -> Model -> List Lexeme
run tokens model =
    let
        initialState =
            START

        loopDetection =
            Dict.empty
    in
    traverse tokens model [ initialState ] loopDetection Nothing 0
