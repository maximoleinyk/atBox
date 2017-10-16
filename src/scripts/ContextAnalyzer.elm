module ContextAnalyzer exposing (run, run2)

import Dict exposing (Dict)
import GlobalTypes exposing (CursorContext(..), Lexeme, LexemeState(Field, Joiner, LeftParenthesis, LexemeValue, Operator, RightParenthesis), Model, OperatorType(IsEitherType, IsInType, IsNeitherType, IsNotInType), QueryField, Token, TokenState(..), ValueType(ValueStringType))
import Regex exposing (HowMany(All))
import Tokenizer
import Utils


getQueryFields : List QueryField -> Dict String String -> Dict String String
getQueryFields queryFields dict =
    case queryFields of
        [] ->
            dict

        next :: rest ->
            let
                newDict =
                    Dict.insert next.field next.label dict
            in
            getQueryFields rest newDict


getLexemeBeforePosition : List Lexeme -> Int -> Maybe Lexeme
getLexemeBeforePosition lexemes position =
    case lexemes of
        [] ->
            Nothing

        [ x ] ->
            if position == 0 then
                Nothing
            else
                Just x

        x :: y :: rest ->
            let
                lexemeFullLength =
                    y.index + String.length y.value
            in
            if position >= lexemeFullLength then
                getLexemeBeforePosition ([ y ] ++ rest) position
            else if position == 0 then
                Nothing
            else
                Just x


getTokenAtCursorPosition : List Token -> Int -> Maybe Token
getTokenAtCursorPosition tokens cursorPosition =
    case tokens of
        [] ->
            Nothing

        current :: rest ->
            let
                fullLength =
                    current.index + String.length current.value
            in
            if cursorPosition > fullLength then
                getTokenAtCursorPosition rest cursorPosition
            else
                Just current


getQueryString : String -> Maybe Token -> Int -> String
getQueryString string token cursorPosition =
    case token of
        Nothing ->
            ""

        Just lexeme ->
            String.slice lexeme.index cursorPosition string


processOperatorsContext : String -> CursorContext
processOperatorsContext query =
    let
        operators =
            Dict.fromList
                [ ( "1", "is" )
                , ( "2", "is in" )
                , ( "3", "is not" )
                , ( "4", "is not in" )
                , ( "5", "is either" )
                , ( "6", "is neither" )
                ]
    in
    OperatorContext operators query


processKeywordContext : Model -> String -> CursorContext
processKeywordContext model query =
    let
        queryFields =
            getQueryFields model.queryFields Dict.empty
    in
    KeywordContext queryFields query


processJoinerContext : String -> String -> CursorContext
processJoinerContext originalString query =
    let
        numberOfLeftBrackets =
            List.length (Regex.find All (Regex.regex "\\(") originalString)

        numberOfRightBrackets =
            List.length (Regex.find All (Regex.regex "\\)") originalString)

        parenthesis =
            if numberOfLeftBrackets - numberOfRightBrackets > 0 then
                [ ( "closing_parenthesis", ")" ) ]
            else
                []

        joiners =
            parenthesis
                ++ [ ( "and", "and" )
                   , ( "or", "or" )
                   ]
    in
    JoinerContext (Dict.fromList joiners) query


getQueryField : List QueryField -> String -> QueryField
getQueryField queryFields fieldName =
    let
        predicate =
            \q -> String.toLower q.field == String.toLower fieldName

        filteredList =
            List.filter predicate queryFields

        maybeField =
            List.head filteredList
    in
    case maybeField of
        Nothing ->
            -- unreachable case
            QueryField "" "" "" []

        Just field ->
            field


getValues : QueryField -> Dict String String
getValues queryField =
    if queryField.fieldType == "enum" then
        Dict.fromList (List.map (\v -> ( v, v )) queryField.values)
    else
        Dict.empty


processValueContext : String -> List Lexeme -> Lexeme -> OperatorType -> Token -> Model -> CursorContext
processValueContext originalString lexemes fieldLexeme operatorType token model =
    let
        queryFieldName : String
        queryFieldName =
            Utils.replace model.keywordDelimiter "" fieldLexeme.value

        queryField : QueryField
        queryField =
            getQueryField model.queryFields queryFieldName

        values : Dict String String
        values =
            getValues queryField

        currentLexemeValue : String
        currentLexemeValue =
            let
                fieldLexeme =
                    getLexemeBeforePosition lexemes (token.index + String.length token.value)
            in
            case fieldLexeme of
                Nothing ->
                    ""

                Just l ->
                    String.trim l.value
    in
    case token.state of
        SpaceTerm ->
            case operatorType of
                IsEitherType ->
                    let
                        l =
                            List.length (String.split " or " currentLexemeValue)
                    in
                    if currentLexemeValue == "" then
                        ValueContext values token.value
                    else if l == 1 then
                        if String.contains " or" currentLexemeValue then
                            ValueContext values token.value
                        else
                            ValueSeparatorContext ValueStringType (Dict.fromList [ ( "or", "or" ) ]) token.value
                    else
                        processJoinerContext originalString token.value

                IsNeitherType ->
                    let
                        l =
                            List.length (String.split " nor " currentLexemeValue)
                    in
                    if currentLexemeValue == "" then
                        ValueContext values token.value
                    else if l == 1 then
                        if String.contains " nor" currentLexemeValue then
                            ValueContext values token.value
                        else
                            ValueSeparatorContext ValueStringType (Dict.fromList [ ( "nor", "nor" ) ]) token.value
                    else
                        processJoinerContext originalString token.value

                IsInType ->
                    ValueSeparatorContext ValueStringType (Dict.fromList [ ( "comma", "," ) ]) token.value

                IsNotInType ->
                    ValueSeparatorContext ValueStringType (Dict.fromList [ ( "comma", "," ) ]) token.value

                _ ->
                    ValueContext values token.value

        WordTerm ->
            ValueContext values token.value

        -- unreachable context
        _ ->
            NoContext


getContext : List Lexeme -> String -> Maybe Token -> Maybe Lexeme -> Int -> Model -> CursorContext
getContext lexemes originalString token lexeme cursorPosition model =
    let
        queryString =
            getQueryString originalString token cursorPosition

        processValue : Lexeme -> Token -> OperatorType -> CursorContext
        processValue =
            \l t operatorType ->
                let
                    fieldLexeme =
                        getLexemeBeforePosition lexemes l.index
                in
                case fieldLexeme of
                    Nothing ->
                        -- unreachable case
                        NoContext

                    Just f ->
                        processValueContext originalString lexemes f operatorType t model
    in
    case token of
        Nothing ->
            case lexeme of
                Nothing ->
                    processKeywordContext model queryString

                _ ->
                    NoContext

        Just token ->
            case token.state of
                UnknownKeywordTerm ->
                    case lexeme of
                        -- "find a person whose @| "
                        Nothing ->
                            processKeywordContext model queryString

                        Just lexeme ->
                            case lexeme.state of
                                -- "... and @| "
                                Joiner ->
                                    processKeywordContext model queryString

                                -- "(@| "
                                LeftParenthesis ->
                                    processKeywordContext model queryString

                                -- unreachable condition
                                _ ->
                                    NoContext

                KeywordTerm ->
                    -- "find a person whose @name|"
                    processKeywordContext model queryString

                WordTerm ->
                    case lexeme of
                        Nothing ->
                            -- "find a person|
                            NoContext

                        Just lexeme ->
                            case lexeme.state of
                                -- "@name word|
                                Field ->
                                    processOperatorsContext queryString

                                -- "@name is message|
                                Operator t ->
                                    processValue lexeme token t

                                -- "@name is Max Oliinyk|
                                LexemeValue ->
                                    processJoinerContext originalString queryString

                                _ ->
                                    NoContext

                SpaceTerm ->
                    case lexeme of
                        -- "      |
                        Nothing ->
                            processKeywordContext model queryString

                        Just lexeme ->
                            case lexeme.state of
                                -- "@name  |
                                Field ->
                                    processOperatorsContext ""

                                LexemeValue ->
                                    -- "@name is Max  |
                                    if lexeme.value /= "" then
                                        processJoinerContext originalString queryString
                                    else
                                        -- "@name is |
                                        let
                                            operatorLexeme =
                                                getLexemeBeforePosition lexemes lexeme.index
                                        in
                                        case operatorLexeme of
                                            Nothing ->
                                                -- unreachable
                                                NoContext

                                            Just lexeme ->
                                                case lexeme.state of
                                                    Operator t ->
                                                        processValue lexeme token t

                                                    _ ->
                                                        -- unreachable
                                                        NoContext

                                -- "@name is Max and    |
                                Joiner ->
                                    processKeywordContext model queryString

                                -- "@name is |
                                -- "@name is not |
                                -- "@name is not in |
                                -- "@name is either |
                                Operator t ->
                                    processValue lexeme token t

                                -- ")   |
                                RightParenthesis ->
                                    processJoinerContext originalString queryString

                                -- "and (  |
                                LeftParenthesis ->
                                    processKeywordContext model queryString

                IsEitherTerm ->
                    case lexeme of
                        -- unreachable
                        Nothing ->
                            NoContext

                        -- "@name is either|
                        Just lexeme ->
                            processOperatorsContext queryString

                IsNeitherTerm ->
                    case lexeme of
                        -- unreachable
                        Nothing ->
                            NoContext

                        -- "@name is neither|
                        Just lexeme ->
                            processOperatorsContext queryString

                IsInTerm ->
                    case lexeme of
                        -- unreachable
                        Nothing ->
                            NoContext

                        Just lexeme ->
                            case lexeme.state of
                                -- "@name is in|
                                -- "@name is not in|
                                Field ->
                                    processOperatorsContext queryString

                                _ ->
                                    NoContext

                IsTerm ->
                    case lexeme of
                        -- unreachable
                        Nothing ->
                            NoContext

                        Just lexeme ->
                            -- "@name is|
                            processOperatorsContext token.value

                IsNotTerm ->
                    case lexeme of
                        Nothing ->
                            NoContext

                        Just lexeme ->
                            -- "@name is not|
                            processOperatorsContext token.value

                OpenParenthesisTerm ->
                    processKeywordContext model queryString

                CloseParenthesisTerm ->
                    processJoinerContext originalString queryString

                OrTerm ->
                    case lexeme of
                        -- unreachable
                        Nothing ->
                            NoContext

                        Just lexeme ->
                            case lexeme.state of
                                -- "@name is Max or|
                                LexemeValue ->
                                    processJoinerContext originalString queryString

                                -- "@name is Max or  |
                                Joiner ->
                                    processKeywordContext model queryString

                                _ ->
                                    NoContext

                AndTerm ->
                    case lexeme of
                        -- unreachable
                        Nothing ->
                            NoContext

                        Just lexeme ->
                            case lexeme.state of
                                -- "@name is Max and|
                                LexemeValue ->
                                    processJoinerContext originalString queryString

                                -- "@name is Max and  |
                                Joiner ->
                                    processKeywordContext model queryString

                                _ ->
                                    NoContext

                _ ->
                    NoContext


run : String -> List Token -> List Lexeme -> Model -> List TokenState -> CursorContext
run string tokens lexemes model remainingStates =
    let
        cursorPosition =
            model.cursorIndex

        tokenAtCursorPosition =
            getTokenAtCursorPosition tokens cursorPosition

        lexemeBeforeToken =
            case tokenAtCursorPosition of
                Nothing ->
                    getLexemeBeforePosition lexemes 0

                Just token ->
                    getLexemeBeforePosition lexemes token.index

        result =
            getContext lexemes string tokenAtCursorPosition lexemeBeforeToken cursorPosition model
    in
    result


getNextNonSpaceTokenState : List TokenState -> Maybe TokenState
getNextNonSpaceTokenState remainingStates =
    case remainingStates of
        [] ->
            Nothing

        next :: rest ->
            case next of
                SpaceTerm ->
                    getNextNonSpaceTokenState rest

                WordTerm ->
                    getNextNonSpaceTokenState rest

                CloseParenthesisTerm ->
                    getNextNonSpaceTokenState rest

                OpenParenthesisTerm ->
                    getNextNonSpaceTokenState rest

                StartQuoteTerm ->
                    getNextNonSpaceTokenState rest

                EndQuoteTerm ->
                    getNextNonSpaceTokenState rest

                _ ->
                    Just next


collectTermsForState : List TokenState -> Int -> List TokenState
collectTermsForState memo itemsToDrop =
    case memo of
        [] ->
            memo

        next :: rest ->
            if Tokenizer.isTermState next then
                if
                    next
                        == SpaceTerm
                        || next
                        == CloseParenthesisTerm
                        || next
                        == OpenParenthesisTerm
                        || next
                        == Statement
                        || next
                        == StartQuoteTerm
                        || next
                        == EndQuoteTerm
                        || next
                        == OpenParenthesisInOperatorTerm
                        || next
                        == CloseParenthesisInOperatorTerm
                then
                    collectTermsForState rest (itemsToDrop + 1)
                else
                    let
                        newMemo =
                            List.drop itemsToDrop rest

                        newLength =
                            List.length newMemo
                    in
                    [ next ] ++ collectTermsForState newMemo newLength
            else
                let
                    newPossibleStates =
                        Tokenizer.getPossibleStates next

                    newMemo =
                        newPossibleStates ++ rest

                    newItemsToDrop =
                        List.length newPossibleStates - 1
                in
                collectTermsForState newMemo newItemsToDrop


getNextPossibleOptions : Maybe TokenState -> List TokenState
getNextPossibleOptions tokenState =
    case tokenState of
        Nothing ->
            []

        Just state ->
            let
                isTermState =
                    Tokenizer.isTermState state
            in
            if isTermState then
                [ state ]
            else
                let
                    possibleStates =
                        Tokenizer.getPossibleStates state
                in
                collectTermsForState possibleStates (List.length possibleStates - 1)


getTokenStateMapping : TokenState -> ( String, String )
getTokenStateMapping tokenState =
    case tokenState of
        StartQuoteTerm ->
            ( "start_quote_term", "\"" )

        EndQuoteTerm ->
            ( "end_quote_term", "\"" )

        EitherOrTerm ->
            ( "either_or_term", "or" )

        AndTerm ->
            ( "and_term", "and" )

        OrTerm ->
            ( "or_term", "or" )

        NeitherNorTerm ->
            ( "neither_nor_term", "nor" )

        IsNotTerm ->
            ( "is_not_term", "is not" )

        IsTerm ->
            ( "is_term", "is" )

        IsEitherTerm ->
            ( "is_either_term", "is neither" )

        IsNeitherTerm ->
            ( "is_neither_term", "is neither" )

        IsInTerm ->
            ( "is_in_term", "is in" )

        IsNotInTerm ->
            ( "is_not_in_term", "is not in" )

        OpenParenthesisInOperatorTerm ->
            ( "open_parenthesis_in_term", "(" )

        CloseParenthesisInOperatorTerm ->
            ( "close_parenthesis_in_term", ")" )

        CommaTerm ->
            ( "comma", "," )

        OpenParenthesisTerm ->
            ( "open_parenthesis_term", "(" )

        CloseParenthesisTerm ->
            ( "close_parenthesis_term", ")" )

        _ ->
            ( "", "" )


convertOperators : List TokenState -> Dict String String -> Dict String String
convertOperators states memo =
    case states of
        [] ->
            memo

        next :: rest ->
            let
                ( key, value ) =
                    getTokenStateMapping next

                newMemo =
                    if key == "" then
                        memo
                    else
                        Dict.insert key value memo
            in
            convertOperators rest newMemo


getContext2 : Maybe TokenState -> Maybe Token -> Maybe Lexeme -> List TokenState -> Model -> CursorContext
getContext2 nextTokenState token lexemeBeforeToken nextPossibleTokenStates model =
    let
        tokenValue =
            case token of
                Nothing ->
                    ""

                Just token ->
                    token.value

        --        a =
        --            Debug.log "tokenValue" tokenValue
        --
        --        b =
        --            Debug.log "nextPossibleTokenStates" nextPossibleTokenStates
    in
    case lexemeBeforeToken of
        Nothing ->
            processKeywordContext model tokenValue

        Just lexemeBefore ->
            let
                lexemeValue =
                    lexemeBefore.value

                convertedOperators =
                    convertOperators nextPossibleTokenStates Dict.empty
            in
            case lexemeBefore.state of
                Field ->
                    OperatorContext convertedOperators tokenValue

                Operator t ->
                    OperatorContext convertedOperators tokenValue

                LexemeValue ->
                    JoinerContext Dict.empty tokenValue

                Joiner ->
                    KeywordContext Dict.empty tokenValue

                LeftParenthesis ->
                    KeywordContext Dict.empty tokenValue

                RightParenthesis ->
                    JoinerContext Dict.empty tokenValue


run2 : String -> List Token -> List Lexeme -> Model -> List TokenState -> CursorContext
run2 originalString tokens lexemes model remainingStates =
    let
        tokenAtCursorPosition =
            getTokenAtCursorPosition tokens model.cursorIndex

        lexemeBeforeToken =
            case tokenAtCursorPosition of
                Nothing ->
                    getLexemeBeforePosition lexemes 0

                Just token ->
                    getLexemeBeforePosition lexemes token.index

        nextNonSpaceTokenState =
            getNextNonSpaceTokenState remainingStates

        suggestItems =
            getNextPossibleOptions nextNonSpaceTokenState

        result =
            getContext2 nextNonSpaceTokenState tokenAtCursorPosition lexemeBeforeToken suggestItems model

        b =
            Debug.log "nextNonSpaceTokenState" nextNonSpaceTokenState

        c =
            Debug.log "tokenAtCursorPosition" tokenAtCursorPosition

        d =
            Debug.log "lexemeBeforeToken" lexemeBeforeToken

        k =
            Debug.log "suggestItems" suggestItems

        f =
            Debug.log "-----------" "-----------"
    in
    result
