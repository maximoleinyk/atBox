module Grammar exposing (run)

import Char
import Parser as P exposing (Parser(..), Problem(..), State(..), Token(..), apply, maybeOne, oneOf, oneOrMore, repeat, sequence, squash, swapBy, word, zeroOrMore)


type Recurrence
    = ZeroOrMore
    | AtLeastOne


type TokenType
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


type alias Expecting =
    List TokenType


type alias State =
    P.State TokenType


type alias Parser =
    P.Parser TokenType


type alias Problem =
    P.Problem TokenType


type alias QueryField =
    { field : String
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


field : Char -> String -> Parser
field char key =
    squash Keyword <|
        sequence
            [ symbol char AtSymbol
            , keyword key Word
            ]


unknownKeyword : Char -> Parser
unknownKeyword char =
    squash UnknownKeyword <|
        sequence
            [ symbol char AtSymbol
            , word Word
            ]


conjunction : Parser
conjunction =
    sequence
        [ spaces ZeroOrMore
        , oneOf
            [ keyword "or" OrJoiner
            , keyword "and" AndJoiner
            ]
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


quotedWord : Parser
quotedWord =
    squash QuotedWord <|
        sequence
            [ doubleQuote
            , repeat <| anythingExceptSymbol '"' QuotedWord
            , doubleQuote
            ]


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
    squash ListValue <|
        sequence
            [ leftParenthesis
            , spaces ZeroOrMore
            , commaSep parser
            , spaces ZeroOrMore
            , rightParenthesis
            ]


stringValue : Parser
stringValue =
    squash StringValue <|
        oneOf
            [ word Word
            , quotedWord
            ]


int : Parser
int =
    let
        mapper =
            \d -> symbol d Char
    in
    squash IntegerValue <|
        sequence
            [ maybeOne <|
                oneOf
                    [ symbol '+' Char
                    , symbol '-' Char
                    ]
            , repeat <| oneOf <| List.map mapper (String.toList "123456789")
            , maybeOne <| repeat <| oneOf <| List.map mapper (String.toList "1234567890")
            ]


float : Parser
float =
    let
        digits =
            List.map (\d -> symbol d Char) (String.toList "1234567890")
    in
    squash FloatValue <|
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
        sequence
            [ keyword "greater" Word
            , spaces AtLeastOne
            , keyword "than" Word
            , spaces AtLeastOne
            , keyword "or" Word
            , spaces AtLeastOne
            , keyword "equals" Word
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
                                    squash EitherOrValue <|
                                        sequence
                                            [ parser
                                            , spaces AtLeastOne
                                            , orOperator
                                            , spaces AtLeastOne
                                            , parser
                                            ]

                                NeitherOperator ->
                                    squash NeitherNorValue <|
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


operatorAndValue : Maybe QueryField -> Parser
operatorAndValue queryField =
    case queryField of
        Just queryField ->
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

        Nothing ->
            repeat <| anythingExceptSymbol ' ' UnknownOperator


keywordAndOperator : List QueryField -> Parser
keywordAndOperator queryFields =
    let
        at =
            '@'

        mapper =
            \queryField ->
                sequence
                    [ field at queryField.label
                    , spaces AtLeastOne
                    , operatorAndValue (Just queryField)
                    ]
    in
    oneOf <| List.map mapper queryFields


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
                                [ StringValue
                                , IntegerValue
                                , FloatValue
                                , ListValue
                                , EitherOrValue
                                , NeitherNorValue
                                , RightParenthesis
                                ]

                    parserTrue =
                        conjunction

                    parserFalse =
                        sequence
                            [ maybeOne <|
                                repeat <|
                                    oneOf
                                        [ word Word
                                        , spaces AtLeastOne
                                        ]
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
            print result

        a =
            Debug.log ">>> " result
    in
    ()


print : Result Problem State -> String
print result =
    let
        helper =
            \items result ->
                case items of
                    [] ->
                        "Empty list"

                    next :: rest ->
                        Debug.log (result ++ " >>> " ++ toString next) (helper rest result)
    in
    case result of
        Ok (State { tokens }) ->
            helper tokens "Ok"

        (Err _) as error ->
            ""
