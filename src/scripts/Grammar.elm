module Grammar exposing (Expecting(..), ParseResult(..), run)

import Char
import ParserUtils as P
    exposing
        ( Parser(..)
        , Problem(..)
        , State(..)
        , Token(..)
        , apply
        , end
        , identity
        , maybeOne
        , oneOf
        , oneOrMore
        , repeat
        , sequence
        , squash
        , word
        , zeroOrMore
        )


type Expecting
    = Expecting (List TokenType)


type ParseResult
    = ParsedSuccessfully (List (Token TokenType))
    | ParseFailed


type Recurrence
    = ZeroOrMore
    | AtLeastOne
    | ExactlyOnce


type TokenType
    = Symbol
    | Word
    | Keyword String
    | Joiner String
    | UnknownOperator
    | Comma
    | IntegerValue
    | FloatValue
    | StringValue
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
    | OpenBracket
    | CloseBracket


type alias State =
    P.State TokenType


type alias Parser =
    P.Parser TokenType


type alias Problem =
    P.Problem TokenType


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
    P.symbol (\c -> Char.toLower c /= Char.toLower char) context


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
    keyword "is" IsOperator


containsOperator : Parser
containsOperator =
    keyword "contains" ContainsOperator


isNotOperator : Parser
isNotOperator =
    squash IsNotOperator <|
        sequence
            [ keyword "is" Word
            , spaces AtLeastOne
            , keyword "not" Word
            ]


stringValue : Parser
stringValue =
    oneOf
        [ word StringValue
        , sequence
            [ doubleQuote
            , squash StringValue <| repeat <| anythingExceptSymbol '"' Symbol
            , doubleQuote
            ]
        ]


int : Parser
int =
    let
        mapper =
            \d -> symbol d Symbol
    in
    squash IntegerValue <|
        sequence
            [ maybeOne <|
                oneOf
                    [ symbol '+' Symbol
                    , symbol '-' Symbol
                    ]
            , repeat <| oneOf <| List.map mapper (String.toList "1234567890")
            ]


float : Parser
float =
    let
        mapper =
            \d -> symbol d Symbol
    in
    squash FloatValue <|
        sequence
            [ maybeOne <|
                oneOf
                    [ symbol '+' Symbol
                    , symbol '-' Symbol
                    ]
            , oneOf
                [ symbol '0' Symbol
                , repeat <| oneOf <| List.map mapper <| String.toList "123456789"
                ]
            , symbol '.' Symbol
            , repeat <| oneOf <| List.map mapper <| String.toList "1234567890"
            ]


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


equals : Parser
equals =
    keyword "equals" EqualsOperator


notEquals : Parser
notEquals =
    squash NotEqualsOperator <|
        sequence
            [ keyword "not" Word
            , spaces AtLeastOne
            , keyword "equals" Word
            ]


lessThan : Parser
lessThan =
    squash LessThanOperator <|
        sequence
            [ keyword "less" Word
            , spaces AtLeastOne
            , keyword "than" Word
            ]


greaterThan : Parser
greaterThan =
    squash GreaterThanOperator <|
        sequence
            [ keyword "greater" Word
            , spaces AtLeastOne
            , keyword "than" Word
            ]


greaterThanOrEquals : Parser
greaterThanOrEquals =
    squash GreaterThanOrEqualsOperator <|
        oneOf
            [ keyword ">=" Word
            , sequence
                [ keyword "greater" Word
                , spaces AtLeastOne
                , keyword "than" Word
                , spaces AtLeastOne
                , keyword "or" Word
                , spaces AtLeastOne
                , keyword "equals" Word
                ]
            ]


lessThanOrEquals : Parser
lessThanOrEquals =
    squash LessThanOrEqualsOperator <|
        sequence
            [ keyword "less" Word
            , spaces AtLeastOne
            , keyword "than" Word
            , spaces AtLeastOne
            , keyword "or" Word
            , spaces AtLeastOne
            , keyword "equals" Word
            ]


isEitherOperator : Parser
isEitherOperator =
    squash EitherOperator <|
        sequence
            [ keyword "is" Word
            , spaces AtLeastOne
            , keyword "either" Word
            ]


isNeitherOperator : Parser
isNeitherOperator =
    squash NeitherOperator <|
        sequence
            [ keyword "is" Word
            , spaces AtLeastOne
            , keyword "neither" Word
            ]


isInOperator : Parser
isInOperator =
    squash IsInOperator <|
        sequence
            [ keyword "is" Word
            , spaces AtLeastOne
            , keyword "in" Word
            ]


unknownOperator : Parser
unknownOperator =
    repeat <| anythingExceptSymbol ' ' UnknownOperator


isNotInOperator : Parser
isNotInOperator =
    squash IsNotInOperator <|
        sequence
            [ keyword "is" Word
            , spaces AtLeastOne
            , keyword "not" Word
            , spaces AtLeastOne
            , keyword "in" Word
            ]


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
    sequence
        [ openBracket
        , oneOf
            [ listHelper parser
            , closeBracket
            ]
        ]


value : Parser -> Parser
value parser =
    Parser <|
        \((State { tokens }) as initialState) ->
            case List.head tokens of
                Just (Token { state }) ->
                    apply initialState <|
                        sequence
                            [ spaces AtLeastOne
                            , case state of
                                EitherOperator ->
                                    sequence
                                        [ parser
                                        , spaces AtLeastOne
                                        , orOperator
                                        , spaces AtLeastOne
                                        , parser
                                        ]

                                NeitherOperator ->
                                    sequence
                                        [ parser
                                        , spaces AtLeastOne
                                        , norOperator
                                        , spaces AtLeastOne
                                        , parser
                                        ]

                                IsInOperator ->
                                    list parser

                                IsNotInOperator ->
                                    list parser

                                _ ->
                                    parser
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
        [ symbol '@' AtSymbol
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

                StringValue ->
                    afterValue level initialParser

                IntegerValue ->
                    afterValue level initialParser

                FloatValue ->
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
            expression
                [ QueryField "name" "name" "string"
                , QueryField "age" "age" "integer"
                , QueryField "status" "status" "enum"
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
            ( Expecting [ Space ], ParsedSuccessfully tokens )

        Err (Problem { expecting }) ->
            ( Expecting expecting, ParseFailed )
