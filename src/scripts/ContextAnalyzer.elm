module ContextAnalyzer exposing (run)

import Dict exposing (Dict)
import GlobalTypes exposing (CursorContext(JoinerContext, KeywordContext, NoContext, OperatorContext, ValueContext, ValueSeparatorContext), Lexeme, LexemeState(Field, Joiner, LeftParenthesis, LexemeValue, Operator, RightParenthesis, UnknownField), Model, OperatorType(IsEitherType, IsInType, IsNeitherType, IsNotInType, IsType), QueryField, Token, TokenState(AndTerm, CloseParenthesisInOperatorTerm, CloseParenthesisTerm, CommaTerm, ContainsTerm, EitherOrTerm, EndQuoteTerm, IsEitherTerm, IsInTerm, IsNeitherTerm, IsNotInTerm, IsNotTerm, IsTerm, KeywordTerm, NeitherNorTerm, OpenParenthesisInOperatorTerm, OpenParenthesisTerm, OrTerm, Sentence, SpaceTerm, StartQuoteTerm, UnknownKeywordTerm, WordTerm), ValueType(ValueStringType))
import Regex exposing (HowMany(All))
import Tokenizer
import Utils exposing (isNothing)


getLexemeBeforePosition : Int -> List Lexeme -> Maybe Lexeme
getLexemeBeforePosition position lexemes =
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

                nextLexemes =
                    if position >= lexemeFullLength then
                        [ y ] ++ rest
                    else
                        [ x ]
            in
            getLexemeBeforePosition position nextLexemes


getTokenAtCursorPosition : Int -> List Token -> Maybe Token
getTokenAtCursorPosition position tokens =
    case tokens of
        [] ->
            Nothing

        [ x ] ->
            if position == 0 then
                Nothing
            else
                Just x

        x :: y :: rest ->
            let
                fullLength =
                    y.index + String.length y.value

                nextLexemes =
                    if position >= fullLength then
                        [ y ] ++ rest
                    else
                        [ x ]
            in
            getTokenAtCursorPosition position nextLexemes


processKeywordContext : Model -> String -> CursorContext
processKeywordContext model query =
    let
        queryFields =
            \fields dict ->
                case fields of
                    [] ->
                        dict

                    next :: rest ->
                        let
                            newDict =
                                Dict.insert next.field next.label dict
                        in
                        queryFields rest newDict
    in
    KeywordContext (queryFields model.queryFields Dict.empty) query


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
                , ( "7", "contains" )
                ]
    in
    OperatorContext operators query


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


processValueContext : String -> List Lexeme -> Model -> Lexeme -> OperatorType -> Token -> CursorContext
processValueContext originalString lexemes model fieldLexeme operatorType token =
    let
        result =
            getLexemeBeforePosition fieldLexeme.index lexemes
    in
    case result of
        Nothing ->
            NoContext

        Just f ->
            let
                queryFieldName =
                    Utils.replace model.keywordDelimiter "" fieldLexeme.value

                queryField =
                    getQueryField model.queryFields queryFieldName

                values =
                    if queryField.fieldType == "enum" then
                        Dict.fromList (List.map (\v -> ( v, v )) queryField.values)
                    else
                        Dict.empty

                currentLexemeValue =
                    let
                        fieldLexeme =
                            getLexemeBeforePosition (token.index + String.length token.value) lexemes
                    in
                    case fieldLexeme of
                        Nothing ->
                            ""

                        Just l ->
                            l.value
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


is : Maybe { a | state : b } -> b -> Bool
is object state =
    case object of
        Nothing ->
            False

        Just o ->
            o.state == state


getLexeme : Maybe Lexeme -> (Lexeme -> a) -> a
getLexeme lexeme func =
    case lexeme of
        Nothing ->
            func (Lexeme Field "" 0)

        Just l ->
            func l


getToken : Maybe Token -> (Token -> a) -> a
getToken token func =
    case token of
        Nothing ->
            func (Token SpaceTerm "" 0)

        Just t ->
            func t


getOperatorType : Maybe Lexeme -> (OperatorType -> b) -> b
getOperatorType lexeme func =
    case lexeme of
        Nothing ->
            func IsType

        Just l ->
            case l.state of
                Operator t ->
                    func t

                _ ->
                    func IsType


isValueEmpty : Maybe Lexeme -> Bool
isValueEmpty lexeme =
    case lexeme of
        Nothing ->
            False

        Just l ->
            l.value == ""


isBeforeEmptyValueWasOperator : List Lexeme -> Maybe Lexeme -> Bool
isBeforeEmptyValueWasOperator lexemes lexeme =
    case lexeme of
        Nothing ->
            False

        Just lexeme ->
            case lexeme.state of
                LexemeValue ->
                    let
                        operatorLexeme =
                            getLexemeBeforePosition lexeme.index lexemes
                    in
                    if lexeme.value == "" then
                        case operatorLexeme of
                            Nothing ->
                                False

                            Just lexeme ->
                                case lexeme.state of
                                    Operator t ->
                                        True

                                    _ ->
                                        False
                    else
                        False

                _ ->
                    False


getContext : List Lexeme -> String -> Maybe Token -> Maybe Lexeme -> String -> Model -> CursorContext
getContext lexemes originalString token lexeme queryString model =
    let
        keywordContextConditions =
            [ isNothing token
            , is token UnknownKeywordTerm
            , is token KeywordTerm
            , is token OpenParenthesisTerm
            , is token SpaceTerm && isNothing lexeme
            , is token SpaceTerm && is lexeme Joiner
            ]

        operatorContextConditions =
            [ is token WordTerm && is lexeme Field
            , is token SpaceTerm && (is lexeme Field || is lexeme UnknownField)
            , is token IsEitherTerm && not (isNothing lexeme)
            , is token IsNeitherTerm && not (isNothing lexeme)
            , is token IsInTerm && is lexeme Field
            , is token IsNotInTerm && is lexeme Field
            , is token IsTerm && not (isNothing lexeme)
            , is token IsNotTerm && not (isNothing lexeme)
            ]

        valueContextConditions =
            [ is token WordTerm && is lexeme (Operator (getOperatorType lexeme (\t -> t)))
            , is token SpaceTerm && is lexeme (Operator (getOperatorType lexeme (\t -> t)))
            , is token SpaceTerm && isBeforeEmptyValueWasOperator lexemes lexeme
            ]

        joinerContextConditions =
            [ is token WordTerm && is lexeme LexemeValue
            , is token SpaceTerm && is lexeme LexemeValue
            , is token SpaceTerm && is lexeme RightParenthesis
            , is token CloseParenthesisTerm
            , is token CloseParenthesisInOperatorTerm
            , is token OrTerm && is lexeme LexemeValue
            , is token AndTerm && is lexeme LexemeValue
            ]

        any list =
            List.any (\f -> f == True) list
    in
    if any keywordContextConditions then
        processKeywordContext model queryString
    else if any operatorContextConditions then
        processOperatorsContext queryString
    else if any valueContextConditions then
        processValueContext originalString lexemes model
            |> getLexeme lexeme
            |> getOperatorType lexeme
            |> getToken token
    else if any joinerContextConditions then
        processJoinerContext originalString queryString
    else
        NoContext


run : String -> List Token -> List Lexeme -> Model -> List TokenState -> CursorContext
run string tokens lexemes model remainingStates =
    let
        cursorPosition =
            model.cursorIndex

        tokenAtCursorPosition =
            getTokenAtCursorPosition cursorPosition tokens

        lexemeBeforeToken =
            case tokenAtCursorPosition of
                Nothing ->
                    Nothing

                Just token ->
                    getLexemeBeforePosition (token.index + String.length token.value) lexemes

        queryString =
            case tokenAtCursorPosition of
                Nothing ->
                    String.slice 0 cursorPosition string

                Just token ->
                    String.slice token.index cursorPosition string

        result =
            getContext lexemes string tokenAtCursorPosition lexemeBeforeToken queryString model
    in
    result
