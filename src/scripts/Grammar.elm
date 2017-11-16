module Grammar exposing (run)

import Parser as P exposing (Parser(..), State(..), Token(..), apply, keyword, maybeOne, oneOf, oneOrMore, repeat, sequence, squash, swapBy, zeroOrMore)


type Recurrence
    = ZeroOrMore
    | AtLeastOne


type Context
    = Char
    | Word
    | QuotedWord
    | Keyword
    | UnknownKeyword
    | OrJoiner
    | AndJoiner
    | UnknownOperator
    | Comma
    | IntegerValue
    | FloatValue
    | ListValue
    | StringValue
    | EitherOrValue
    | NeitherNorValue
    | Space
    | LeftParenthesis
    | RightParenthesis
    | Quote
    | AtSymbol
    | IsOperator
    | IsInOperator
    | IsNotInOperator
    | IsNotOperator
    | ContainsOperator
    | EqualsOperator
    | NotEqualsOperator
    | LessThanOperator
    | GreaterThanOperator
    | GreaterThanOrEqualsOperator
    | LessThanOrEqualsOperator
    | EitherOperator
    | NeitherOperator
    | OrOperator
    | NorOperator


type alias State =
    P.State Context


type alias Parser =
    P.Parser Context


type alias QueryField =
    { field : String
    , label : String
    , fieldType : String
    }


symbol : Char -> Context -> Parser
symbol char context =
    P.symbol (\c -> c == char) context


anythingExceptSymbol : Char -> Context -> Parser
anythingExceptSymbol char context =
    P.symbol (\c -> c /= char) context


comma : Parser
comma =
    symbol ',' Comma


leftParenthesis : Parser
leftParenthesis =
    symbol '(' LeftParenthesis


rightParenthesis : Parser
rightParenthesis =
    symbol ')' RightParenthesis


doubleQuote : Parser
doubleQuote =
    symbol '"' Quote


spaces : Recurrence -> Parser
spaces recurrence =
    Parser <|
        \initialState ->
            let
                result =
                    apply initialState <|
                        case recurrence of
                            ZeroOrMore ->
                                zeroOrMore (symbol ' ' Space)

                            AtLeastOne ->
                                oneOrMore (symbol ' ' Space)
            in
            case result of
                Ok nextState ->
                    Ok (squash initialState nextState Space)

                (Err x) as result ->
                    result


word : Parser
word =
    Parser <|
        \((State { source, offset, tokens }) as state) ->
            let
                getNewOffset =
                    \offset ->
                        let
                            alphabet =
                                String.split "" "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

                            subString =
                                String.slice offset (offset + 1) source
                        in
                        if List.member subString alphabet then
                            getNewOffset (offset + 1)
                        else
                            offset

                newOffset =
                    getNewOffset offset
            in
            if newOffset > offset then
                let
                    value =
                        String.slice offset newOffset source

                    token =
                        Token
                            { state = Word
                            , position = offset
                            , value = value
                            }
                in
                Ok
                    (State
                        { source = source
                        , offset = newOffset
                        , tokens = token :: tokens
                        }
                    )
            else
                Err "Cannot parse a word"


field : Char -> String -> Parser
field char key =
    Parser <|
        \initialState ->
            let
                result =
                    apply initialState <|
                        sequence
                            [ symbol char AtSymbol
                            , keyword key Word
                            ]
            in
            case result of
                Ok newState ->
                    Ok (squash initialState newState Keyword)

                (Err x) as result ->
                    result


unknownKeyword : Char -> Parser
unknownKeyword char =
    Parser <|
        \initialState ->
            let
                result =
                    apply initialState <|
                        sequence
                            [ symbol char AtSymbol
                            , word
                            ]
            in
            case result of
                Ok newState ->
                    Ok (squash initialState newState UnknownKeyword)

                (Err _) as result ->
                    result


conjunction : Parser
conjunction =
    oneOf
        [ keyword "or" OrJoiner
        , keyword "and" AndJoiner
        ]


isOperator : Parser
isOperator =
    keyword "is" IsOperator


containsOperator : Parser
containsOperator =
    keyword "contains" ContainsOperator


isNotOperator : Parser
isNotOperator =
    Parser <|
        \initialState ->
            let
                result =
                    apply initialState <|
                        sequence
                            [ keyword "is" Word
                            , spaces AtLeastOne
                            , keyword "not" Word
                            ]
            in
            case result of
                Ok newState ->
                    Ok (squash initialState newState IsNotOperator)

                (Err x) as result ->
                    result


quotedWord : Parser
quotedWord =
    Parser <|
        \initialState ->
            let
                result =
                    apply initialState <|
                        sequence
                            [ doubleQuote
                            , repeat <| anythingExceptSymbol '"' QuotedWord
                            , doubleQuote
                            ]
            in
            case result of
                Ok newState ->
                    Ok (squash initialState newState QuotedWord)

                (Err _) as result ->
                    result


commaSep : Parser -> Parser
commaSep parser =
    Parser <|
        \initialState ->
            let
                condition =
                    \tokenType ->
                        List.member tokenType [ StringValue, IntegerValue, FloatValue ]

                parserTrue =
                    sequence
                        [ spaces ZeroOrMore
                        , comma
                        ]

                parserFalse =
                    sequence
                        [ spaces ZeroOrMore
                        , parser
                        ]

                result =
                    apply initialState <|
                        swapBy condition parserTrue parserFalse
            in
            result


list : Parser -> Parser
list parser =
    Parser <|
        \initialState ->
            let
                result =
                    apply initialState <|
                        sequence
                            [ leftParenthesis
                            , spaces ZeroOrMore
                            , commaSep parser
                            , spaces ZeroOrMore
                            , rightParenthesis
                            ]
            in
            case result of
                Ok newState ->
                    Debug.log "list:" <| Ok (squash initialState newState ListValue)

                (Err _) as result ->
                    result


stringValue : Parser
stringValue =
    Parser <|
        \initialState ->
            let
                result =
                    apply initialState <|
                        oneOf
                            [ word
                            , quotedWord
                            ]
            in
            case result of
                Ok newState ->
                    Ok (squash initialState newState StringValue)

                (Err _) as result ->
                    result


int : Parser
int =
    Parser <|
        \initialState ->
            let
                mapper =
                    \d -> symbol d Char

                result =
                    apply initialState <|
                        sequence
                            [ maybeOne <|
                                oneOf
                                    [ symbol '+' Char
                                    , symbol '-' Char
                                    ]
                            , repeat <| oneOf <| List.map mapper (String.toList "123456789")
                            , maybeOne <| repeat <| oneOf <| List.map mapper (String.toList "1234567890")
                            ]
            in
            case result of
                Ok newState ->
                    Ok (squash initialState newState IntegerValue)

                (Err _) as result ->
                    result


float : Parser
float =
    Parser <|
        \initialState ->
            let
                digits =
                    List.map (\d -> symbol d Char) (String.toList "1234567890")

                result =
                    apply initialState <|
                        sequence
                            [ maybeOne <|
                                oneOf
                                    [ symbol '+' Char
                                    , symbol '-' Char
                                    ]
                            , repeat <| oneOf digits
                            , symbol '.' Char
                            , repeat <| oneOf digits
                            ]
            in
            case result of
                Ok newState ->
                    Ok (squash initialState newState FloatValue)

                (Err _) as result ->
                    result


numberValue : Parser
numberValue =
    oneOf
        [ float
        , int
        ]


orOperator : Parser
orOperator =
    keyword "or" OrOperator


norOperator : Parser
norOperator =
    keyword "nor" NorOperator


value : Parser -> Parser
value parser =
    Parser <|
        \((State { tokens }) as initialState) ->
            case List.head tokens of
                Just (Token { state }) ->
                    case state of
                        EitherOperator ->
                            let
                                result =
                                    apply initialState <|
                                        sequence
                                            [ spaces AtLeastOne
                                            , parser
                                            , spaces AtLeastOne
                                            , orOperator
                                            , spaces AtLeastOne
                                            , parser
                                            ]
                            in
                            case result of
                                Ok newState ->
                                    Ok (squash initialState newState EitherOrValue)

                                (Err _) as result ->
                                    result

                        NeitherOperator ->
                            let
                                result =
                                    apply initialState <|
                                        sequence
                                            [ spaces AtLeastOne
                                            , parser
                                            , spaces AtLeastOne
                                            , norOperator
                                            , spaces AtLeastOne
                                            , parser
                                            ]
                            in
                            case result of
                                Ok newState ->
                                    Ok (squash initialState newState NeitherNorValue)

                                (Err _) as result ->
                                    result

                        IsInOperator ->
                            apply initialState <|
                                sequence
                                    [ spaces AtLeastOne
                                    , list parser
                                    ]

                        IsNotInOperator ->
                            apply initialState <|
                                sequence
                                    [ spaces AtLeastOne
                                    , list parser
                                    ]

                        _ ->
                            apply initialState <|
                                sequence
                                    [ spaces AtLeastOne
                                    , parser
                                    ]

                Nothing ->
                    Ok initialState


equals : Parser
equals =
    keyword "equals" EqualsOperator


notEquals : Parser
notEquals =
    Parser <|
        \initialState ->
            let
                result =
                    apply initialState <|
                        sequence
                            [ keyword "not" Word
                            , spaces AtLeastOne
                            , keyword "equals" Word
                            ]
            in
            case result of
                Ok newState ->
                    Ok (squash initialState newState NotEqualsOperator)

                (Err x) as result ->
                    result


lessThan : Parser
lessThan =
    Parser <|
        \initialState ->
            let
                result =
                    apply initialState <|
                        sequence
                            [ keyword "less" Word
                            , spaces AtLeastOne
                            , keyword "than" Word
                            ]
            in
            case result of
                Ok newState ->
                    Ok (squash initialState newState LessThanOperator)

                (Err x) as result ->
                    result


greaterThan : Parser
greaterThan =
    Parser <|
        \initialState ->
            let
                result =
                    apply initialState <|
                        sequence
                            [ keyword "greater" Word
                            , spaces AtLeastOne
                            , keyword "than" Word
                            ]
            in
            case result of
                Ok newState ->
                    Ok (squash initialState newState GreaterThanOperator)

                (Err x) as result ->
                    result


greaterThanOrEquals : Parser
greaterThanOrEquals =
    Parser <|
        \initialState ->
            let
                result =
                    apply initialState <|
                        sequence
                            [ keyword "greater" Word
                            , spaces AtLeastOne
                            , keyword "than" Word
                            , spaces AtLeastOne
                            , keyword "or" Word
                            , spaces AtLeastOne
                            , keyword "equals" Word
                            ]
            in
            case result of
                Ok newState ->
                    Ok (squash initialState newState GreaterThanOrEqualsOperator)

                (Err x) as result ->
                    result


lessThanOrEquals : Parser
lessThanOrEquals =
    Parser <|
        \initialState ->
            let
                result =
                    apply initialState <|
                        sequence
                            [ keyword "less" Word
                            , spaces AtLeastOne
                            , keyword "than" Word
                            , spaces AtLeastOne
                            , keyword "or" Word
                            , spaces AtLeastOne
                            , keyword "equals" Word
                            ]
            in
            case result of
                Ok newState ->
                    Ok (squash initialState newState LessThanOrEqualsOperator)

                (Err x) as result ->
                    result


isEitherOperator : Parser
isEitherOperator =
    Parser <|
        \initialState ->
            let
                result =
                    apply initialState <|
                        sequence
                            [ keyword "is" Word
                            , spaces AtLeastOne
                            , keyword "either" Word
                            ]
            in
            case result of
                Ok newState ->
                    Ok (squash initialState newState EitherOperator)

                (Err x) as result ->
                    result


isNeitherOperator : Parser
isNeitherOperator =
    Parser <|
        \initialState ->
            let
                result =
                    apply initialState <|
                        sequence
                            [ keyword "is" Word
                            , spaces AtLeastOne
                            , keyword "neither" Word
                            ]
            in
            case result of
                Ok newState ->
                    Ok (squash initialState newState NeitherOperator)

                (Err x) as result ->
                    result


isInOperator : Parser
isInOperator =
    Parser <|
        \initialState ->
            let
                result =
                    apply initialState <|
                        sequence
                            [ keyword "is" Word
                            , spaces AtLeastOne
                            , keyword "in" Word
                            ]
            in
            case result of
                Ok newState ->
                    Ok (squash initialState newState IsInOperator)

                (Err x) as result ->
                    result


isNotInOperator : Parser
isNotInOperator =
    Parser <|
        \initialState ->
            let
                result =
                    apply initialState <|
                        sequence
                            [ keyword "is" Word
                            , spaces AtLeastOne
                            , keyword "not" Word
                            , spaces AtLeastOne
                            , keyword "in" Word
                            ]
            in
            case result of
                Ok newState ->
                    Ok (squash initialState newState IsNotInOperator)

                (Err x) as result ->
                    result


operatorAndValue : QueryField -> Parser
operatorAndValue queryField =
    case queryField.fieldType of
        "string" ->
            sequence
                [ oneOf
                    [ isNotInOperator
                    , isNotOperator
                    , isEitherOperator
                    , containsOperator
                    , isNeitherOperator
                    , isInOperator
                    , isOperator
                    ]
                , value stringValue
                ]

        "number" ->
            sequence
                [ oneOf
                    [ isNotInOperator
                    , notEquals
                    , equals
                    , greaterThanOrEquals
                    , lessThanOrEquals
                    , greaterThan
                    , lessThan
                    , isInOperator
                    , isEitherOperator
                    , isNeitherOperator
                    ]
                , value numberValue
                ]

        _ ->
            repeat <| anythingExceptSymbol ' ' UnknownOperator


keywordAndOperator : List QueryField -> Parser
keywordAndOperator queryFields =
    Parser <|
        \initialState ->
            let
                at =
                    '@'

                result =
                    apply initialState <|
                        oneOf
                            [ oneOf (List.map (\f -> field at f.label) queryFields)
                            , unknownKeyword at
                            ]
            in
            case result of
                Ok (State nextState) ->
                    case List.head nextState.tokens of
                        Just (Token { state, value }) ->
                            case state of
                                Keyword ->
                                    let
                                        predicate =
                                            \f -> String.toLower (String.fromChar at ++ f.field) == String.toLower value

                                        queryField =
                                            List.head (List.filter predicate queryFields)
                                    in
                                    case queryField of
                                        Just queryField ->
                                            let
                                                parser =
                                                    sequence
                                                        [ spaces AtLeastOne
                                                        , operatorAndValue queryField
                                                        ]
                                            in
                                            apply (State nextState) parser

                                        Nothing ->
                                            Err ("Could not find query field " ++ value)

                                _ ->
                                    Err "Last parsed token was not a Keyword"

                        Nothing ->
                            Err "Operator group no available tokens"

                (Err x) as result ->
                    result


criteria : Bool -> Parser
criteria flag =
    oneOf
        [ sequence
            [ leftParenthesis
            , spaces ZeroOrMore
            , start flag
            , spaces ZeroOrMore
            , rightParenthesis
            ]
        , keywordAndOperator
            [ QueryField "name" "name" "string"
            , QueryField "age" "age" "number"
            , QueryField "status" "status" "enum"
            ]
        ]


start : Bool -> Parser
start flag =
    Parser <|
        \initialState ->
            if flag then
                Ok initialState
            else
                let
                    condition =
                        \tokenType ->
                            List.member tokenType
                                [ IntegerValue
                                , FloatValue
                                , ListValue
                                , StringValue
                                , Quote
                                , RightParenthesis
                                , EitherOrValue
                                , NeitherNorValue
                                ]

                    parserTrue =
                        sequence
                            [ spaces ZeroOrMore
                            , conjunction
                            ]

                    parserFalse =
                        sequence
                            [ maybeOne <| repeat <| oneOf [ word, spaces AtLeastOne ]
                            , criteria False
                            ]

                    result =
                        apply initialState <|
                            swapBy condition parserTrue parserFalse
                in
                result


run : String -> ()
run source =
    let
        initialState =
            State
                { source = source
                , offset = 0
                , tokens = []
                }

        result =
            apply initialState <|
                start False

        _ =
            Debug.log "result" (reverseTokens result)
    in
    ()


reverseTokens : Result String State -> Result String State
reverseTokens result =
    case result of
        Ok (State state) ->
            Ok (State { state | tokens = List.reverse state.tokens })

        Err x ->
            Err x
