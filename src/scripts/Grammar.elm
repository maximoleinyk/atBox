module Grammar exposing (run)

import Parser as P
    exposing
        ( Parser(..)
        , State(..)
        , Token(..)
        , apply
        , keyword
        , maybeOne
        , oneOf
        , oneOrMore
        , parseAndFlip
        , repeat
        , sequence
        , squash
        , zeroOrMore
        )


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
    | NumberOperator
    | StringOperator
    | UnknownOperator
    | Sentence
    | Comma
    | IntegerValue
    | FloatValue
    | ListValue
    | StringValue
    | Space
    | LeftParenthesis
    | RightParenthesis
    | Quote
    | AtSymbol
    | IsOperator
    | IsNotOperator
    | ContainsOperator
    | EqualsOperator
    | NotEqualsOperator
    | LessThanOperator
    | GreaterThanOperator
    | GreaterThanOrEqualsOperator
    | LessThanOrEqualsOperator


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


sentence : Parser
sentence =
    Parser <|
        \initialState ->
            let
                result =
                    apply initialState <|
                        repeat
                            (oneOf
                                [ word
                                , spaces AtLeastOne
                                ]
                            )
            in
            case result of
                Ok newState ->
                    Ok (squash initialState newState Sentence)

                (Err x) as result ->
                    result


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
                        parseAndFlip condition parserTrue parserFalse
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
                    Ok (squash initialState newState ListValue)

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
            Ok initialState


numberValue : Parser
numberValue =
    oneOf
        [ int
        , float
        ]


value : Parser -> Parser
value parser =
    sequence
        [ spaces AtLeastOne
        , oneOf
            [ parser
            , list parser
            ]
        ]


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


operator : QueryField -> Parser
operator queryField =
    case queryField.fieldType of
        "string" ->
            sequence
                [ oneOf
                    [ isOperator
                    , isNotOperator
                    , containsOperator
                    ]
                , value stringValue
                ]

        "number" ->
            sequence
                [ oneOf
                    [ equals
                    , notEquals
                    , greaterThan
                    , greaterThanOrEquals
                    , lessThan
                    , lessThanOrEquals
                    ]
                , value numberValue
                ]

        _ ->
            repeat <| anythingExceptSymbol ' ' UnknownOperator


operatorGroup : List QueryField -> Parser
operatorGroup queryFields =
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
                                                        , operator queryField
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


criteria : Parser
criteria =
    oneOf
        [ sequence
            [ leftParenthesis
            , spaces ZeroOrMore
            , start
            , spaces ZeroOrMore
            , rightParenthesis
            ]
        , operatorGroup
            [ QueryField "name" "name" "string"
            , QueryField "age" "age" "number"
            , QueryField "status" "status" "enum"
            ]
        ]


start : Parser
start =
    Parser <|
        \initialState ->
            let
                result =
                    apply initialState <|
                        sequence
                            [ maybeOne sentence
                            , criteria
                            ]
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
                int

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
