module Grammar
    exposing
        ( OperatorType(..)
        , ParseResult(..)
        , TokenType(..)
        , ValueType(..)
        , run
        )

import Char
import ParserUtils as P
    exposing
        ( Expecting(..)
        , Parser(..)
        , Problem(..)
        , State(..)
        , Token(..)
        , apply
        , end
        , maybeOne
        , oneOf
        , oneOrMore
        , repeat
        , sequence
        , squash
        , zeroOrMore
        )


type ParseResult
    = ParsedSuccessfully (List (Token TokenType))
    | ParseFailed


type Recurrence
    = ZeroOrMore
    | AtLeastOne
    | ExactlyOnce


type ValueType
    = IntegerType
    | StringType
    | FloatType
    | ListValue
    | EitherValue
    | NeitherValue


type OperatorType
    = IsType
    | IsNotType
    | IsInType
    | IsNotInType
    | IsEitherType
    | IsNeitherType
    | IsLessThanType
    | IsLessThanOrEqualsType
    | IsGreaterThan
    | IsGreaterThanOrEqualsType
    | EqualsType
    | NotEqualsType
    | ContainsType
    | UnknownType


type TokenType
    = Symbol
    | Word
    | OpenBracket
    | CloseBracket
    | Comma
    | Space
    | LeftParenthesis
    | RightParenthesis
    | Quote
    | AtSymbol
    | Nor
    | Or
    | Keyword String
    | Operator OperatorType
    | Value ValueType
    | Joiner String


type alias State =
    P.State TokenType


type alias Parser =
    P.Parser TokenType


type alias Problem =
    P.Problem TokenType


type alias Expecting =
    P.Expecting TokenType


type alias QueryField =
    { id : String
    , label : String
    , fieldType : String
    }


symbol : Char -> TokenType -> Parser
symbol char context =
    P.symbol (\c -> Char.toLower c == Char.toLower char) context


anythingExceptSymbol : Char -> TokenType -> Parser
anythingExceptSymbol char context =
    P.symbol (\c -> c /= char) context


keyword : String -> TokenType -> Parser
keyword key context =
    let
        mapper =
            \char -> symbol char context

        chars =
            String.toList <| String.toLower key
    in
    squash context <|
        sequence <|
            List.map mapper chars


at : Parser
at =
    symbol '@' AtSymbol


comma : Parser
comma =
    symbol ',' Comma


leftParenthesis : Parser
leftParenthesis =
    symbol '(' LeftParenthesis


rightParenthesis : Parser
rightParenthesis =
    symbol ')' RightParenthesis


openBracket : Parser
openBracket =
    symbol '[' OpenBracket


closeBracket : Parser
closeBracket =
    symbol ']' CloseBracket


doubleQuote : Parser
doubleQuote =
    symbol '"' Quote


spaces : Recurrence -> Parser
spaces recurrence =
    squash Space <|
        case recurrence of
            ZeroOrMore ->
                zeroOrMore (symbol ' ' Space)

            AtLeastOne ->
                oneOrMore (symbol ' ' Space)

            ExactlyOnce ->
                symbol ' ' Space


conjunction : Parser
conjunction =
    oneOf
        [ keyword "or" <| Joiner "or"
        , keyword "and" <| Joiner "and"
        ]


isOperator : Parser
isOperator =
    let
        context =
            Operator IsType
    in
    oneOf
        [ keyword "is" context
        , keyword "=" context
        ]


containsOperator : Parser
containsOperator =
    keyword "contains" (Operator ContainsType)


isNotOperator : Parser
isNotOperator =
    let
        context =
            Operator IsNotType
    in
    squash context <|
        sequence
            [ keyword "is" context
            , spaces AtLeastOne
            , keyword "not" context
            ]


nonBreakingWord : TokenType -> Parser
nonBreakingWord context =
    squash context <| repeat <| anythingExceptSymbol ' ' context


stringValue : Parser
stringValue =
    oneOf
        [ nonBreakingWord (Value StringType)
        , sequence
            [ doubleQuote
            , squash (Value StringType) <| repeat <| anythingExceptSymbol '"' Symbol
            , doubleQuote
            ]
        ]


int : Parser
int =
    let
        context =
            Value IntegerType

        mapper =
            \d -> symbol d context
    in
    squash (Value IntegerType) <|
        sequence
            [ maybeOne <|
                oneOf
                    [ symbol '+' context
                    , symbol '-' context
                    ]
            , repeat <| oneOf <| List.map mapper (String.toList "1234567890")
            ]


float : Parser
float =
    let
        context =
            Value FloatType

        mapper =
            \d -> symbol d context
    in
    squash (Value FloatType) <|
        sequence
            [ maybeOne <|
                oneOf
                    [ symbol '+' context
                    , symbol '-' context
                    ]
            , oneOf
                [ symbol '0' context
                , repeat <| oneOf <| List.map mapper <| String.toList "123456789"
                ]
            , symbol '.' context
            , repeat <| oneOf <| List.map mapper <| String.toList "1234567890"
            ]


numberValue : Parser
numberValue =
    oneOf
        [ float
        , int
        ]


or : Parser
or =
    keyword "or" Or


nor : Parser
nor =
    keyword "nor" Nor


equals : Parser
equals =
    keyword "equals" (Operator EqualsType)


notEquals : Parser
notEquals =
    let
        context =
            Operator NotEqualsType
    in
    squash context <|
        sequence
            [ keyword "not" context
            , spaces AtLeastOne
            , keyword "equals" context
            ]


lessThan : Parser
lessThan =
    let
        context =
            Operator IsLessThanType
    in
    squash context <|
        sequence
            [ keyword "less" context
            , spaces AtLeastOne
            , keyword "than" context
            ]


greaterThan : Parser
greaterThan =
    let
        context =
            Operator IsGreaterThan
    in
    squash context <|
        sequence
            [ keyword "greater" context
            , spaces AtLeastOne
            , keyword "than" context
            ]


greaterThanOrEquals : Parser
greaterThanOrEquals =
    let
        context =
            Operator IsGreaterThanOrEqualsType
    in
    squash context <|
        oneOf
            [ keyword ">=" context
            , sequence
                [ keyword "greater" context
                , spaces AtLeastOne
                , keyword "than" context
                , spaces AtLeastOne
                , keyword "or" context
                , spaces AtLeastOne
                , keyword "equals" context
                ]
            ]


lessThanOrEquals : Parser
lessThanOrEquals =
    let
        context =
            Operator IsLessThanOrEqualsType
    in
    squash context <|
        sequence
            [ keyword "less" context
            , spaces AtLeastOne
            , keyword "than" context
            , spaces AtLeastOne
            , keyword "or" context
            , spaces AtLeastOne
            , keyword "equals" context
            ]


isEitherOperator : Parser
isEitherOperator =
    let
        context =
            Operator IsEitherType
    in
    squash context <|
        sequence
            [ keyword "is" context
            , spaces AtLeastOne
            , keyword "either" context
            ]


isNeitherOperator : Parser
isNeitherOperator =
    let
        context =
            Operator IsNeitherType
    in
    squash context <|
        sequence
            [ keyword "is" context
            , spaces AtLeastOne
            , keyword "neither" context
            ]


isInOperator : Parser
isInOperator =
    let
        context =
            Operator IsInType
    in
    squash context <|
        sequence
            [ keyword "is" context
            , spaces AtLeastOne
            , keyword "in" context
            ]


isNotInOperator : Parser
isNotInOperator =
    let
        context =
            Operator IsNotInType
    in
    squash context <|
        sequence
            [ keyword "is" context
            , spaces AtLeastOne
            , keyword "not" context
            , spaces AtLeastOne
            , keyword "in" context
            ]


unknownOperator : Parser
unknownOperator =
    repeat <| anythingExceptSymbol ' ' (Operator UnknownType)


list : Parser -> Parser
list parser =
    let
        listHelper parser =
            Parser <|
                \initialState ->
                    let
                        p =
                            sequence
                                [ spaces ZeroOrMore
                                , parser
                                ]
                    in
                    case apply initialState p of
                        Ok nextState ->
                            apply nextState <|
                                sequence
                                    [ spaces ZeroOrMore
                                    , oneOf
                                        [ closeBracket
                                        , sequence
                                            [ comma
                                            , listHelper parser
                                            ]
                                        ]
                                    ]

                        (Err x) as error ->
                            error
    in
    squash (Value ListValue) <|
        sequence
            [ openBracket
            , oneOf
                [ listHelper parser
                , closeBracket
                ]
            ]


valueHelper : TokenType -> Parser -> Parser
valueHelper state parser =
    case state of
        Operator operatorType ->
            case operatorType of
                IsEitherType ->
                    squash (Value EitherValue) <|
                        sequence
                            [ parser
                            , spaces AtLeastOne
                            , or
                            , spaces AtLeastOne
                            , parser
                            ]

                IsNeitherType ->
                    squash (Value NeitherValue) <|
                        sequence
                            [ parser
                            , spaces AtLeastOne
                            , nor
                            , spaces AtLeastOne
                            , parser
                            ]

                IsInType ->
                    list parser

                IsNotInType ->
                    list parser

                _ ->
                    parser

        _ ->
            parser


value : Parser -> Parser
value parser =
    Parser <|
        \((State { tokens }) as initialState) ->
            case List.head tokens of
                Just (Token { state }) ->
                    apply initialState <|
                        sequence
                            [ spaces AtLeastOne
                            , valueHelper state parser
                            ]

                Nothing ->
                    Ok initialState


stringOperators : Parser
stringOperators =
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


numberOperators : Parser
numberOperators =
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


expression : List QueryField -> Parser
expression queryFields =
    let
        operator =
            \queryField ->
                case queryField.fieldType of
                    "string" ->
                        stringOperators

                    "integer" ->
                        numberOperators

                    "float" ->
                        numberOperators

                    _ ->
                        unknownOperator

        mapper =
            \queryField ->
                sequence
                    [ keyword queryField.label <| Keyword queryField.id
                    , spaces AtLeastOne
                    , operator queryField
                    ]
    in
    sequence
        [ at
        , oneOf <| List.map mapper queryFields
        ]


getNextParser : Int -> List (Token TokenType) -> Parser -> Parser
getNextParser level items initialParser =
    case items of
        [] ->
            Debug.crash "wrong logic"

        (Token next) :: rest ->
            case next.state of
                LeftParenthesis ->
                    afterLeftParenthesis (level + 1) initialParser

                RightParenthesis ->
                    afterRightParenthesis (level - 1) initialParser

                Joiner _ ->
                    startHelper level initialParser <|
                        expressionGroup initialParser

                Value _ ->
                    afterValue level initialParser

                _ ->
                    getNextParser level rest initialParser


afterLeftParenthesis : Int -> Parser -> Parser
afterLeftParenthesis level initialParser =
    sequence
        [ spaces ZeroOrMore
        , startHelper level initialParser <|
            expressionGroup initialParser
        ]


afterRightParenthesis : Int -> Parser -> Parser
afterRightParenthesis level initialParser =
    let
        parser =
            if level > 0 then
                startHelper level initialParser rightParenthesis
            else
                spaces ZeroOrMore
    in
    oneOf
        [ sequence
            [ spaces AtLeastOne
            , startHelper level initialParser conjunction
            ]
        , parser
        ]


afterValue : Int -> Parser -> Parser
afterValue level initialParser =
    if level > 0 then
        startHelper level initialParser <|
            oneOf
                [ sequence
                    [ spaces AtLeastOne
                    , oneOf [ conjunction, rightParenthesis ]
                    ]
                , sequence
                    [ spaces ZeroOrMore
                    , rightParenthesis
                    ]
                ]
    else
        oneOf
            [ sequence
                [ spaces AtLeastOne
                , startHelper level initialParser conjunction
                ]
            , spaces ZeroOrMore
            ]


expressionGroup : Parser -> Parser
expressionGroup initialParser =
    sequence
        [ spaces ZeroOrMore
        , oneOf
            [ initialParser
            , leftParenthesis
            ]
        ]


startHelper : Int -> Parser -> Parser -> Parser
startHelper level initialParser parser =
    Parser <|
        \initialState ->
            case apply initialState parser of
                Ok ((State { tokens }) as nextState) ->
                    apply nextState <|
                        getNextParser level tokens initialParser

                (Err x) as error ->
                    error


start : Parser
start =
    let
        initialParser =
            sequence
                [ spaces ZeroOrMore
                , expression
                    [ QueryField "name" "name" "string"
                    , QueryField "age" "age" "integer"
                    , QueryField "status" "status" "enum"
                    ]
                ]
    in
    startHelper 0 initialParser <|
        expressionGroup initialParser


run : String -> ( Expecting, ParseResult )
run source =
    let
        initialState =
            State
                { source = source
                , offset = 0
                , tokens = []
                }

        result =
            apply initialState <| sequence [ start, end ]
    in
    case result of
        Ok (State { tokens }) ->
            ( ExpectingSingle Space, ParsedSuccessfully <| List.reverse tokens )

        Err (Problem { expecting }) ->
            ( expecting, ParseFailed )
