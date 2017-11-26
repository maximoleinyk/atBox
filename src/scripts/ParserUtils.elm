module ParserUtils
    exposing
        ( Expecting(..)
        , Parser(..)
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
        , symbol
        , zeroOrMore
        )

{-| A utility library for writing custom parsers.


# Types

@docs Expecting, Token, State, Problem, Parser


# Basic parsers

@docs identity, symbol, end


# Utility functions

@docs maybeOne, oneOrMore, zeroOrMore, oneOf, sequence, repeat, squash, apply

-}


{-| Defines expecting token that should be parsed next in the sequence.
-}
type Expecting a
    = ExpectingEnd
    | ExpectingSingle a
    | ExpectingList (List a)


{-| A type which represents parsed token.
-}
type Token a
    = Token
        { state : a
        , position : Int
        , value : String
        }


{-| A type which represents the state of parsed text
-}
type State a
    = State
        { source : String
        , offset : Int
        , tokens : List (Token a)
        }


{-| Problem represents an issue with issue parsed text
-}
type Problem a
    = Problem
        { latestState : State a
        , expecting : Expecting a
        , offset : Int
        }


{-| Basic type which defined a parser. Returns (Err Problem) in case of failed
parse operation and (Ok State) in case of successfully parsed text.

Each parser takes state and produces some result. Result can be either
positive or negative. Positive means provided text was parsed successfully
or negative - parse operation was failed. In case of success there will be
a new state which would be returns as (Ok State). In case of fail Problem
type would be returned.

-}
type Parser a
    = Parser (State a -> Result (Problem a) (State a))


{-| Calls parser with a given state.

    let
        initialState =
            State
                { source = source
                , offset = 0
                , tokens = []
                }
        parser =
            Parser <| \state -> Ok state

    in
    apply state parser

-}
apply : State a -> Parser a -> Result (Problem a) (State a)
apply state (Parser parse) =
    parse state


{-| Parser that defines end of the parsed string.

    let
        initialState =
            State
                { source = source
                , offset = 0
                , tokens = []
                }
    in
    apply initialState <| sequence [ start, end ]

-}
end : Parser a
end =
    Parser <|
        \((State { source, offset }) as initialState) ->
            if String.length source /= offset then
                Err <|
                    Problem
                        { latestState = initialState
                        , expecting = ExpectingEnd
                        , offset = offset
                        }
            else
                Ok initialState


{-| Single character parser.

    space : Parser TokenType
    space =
        symbol (\c -> c == ' ') Space

    keyword : String -> TokenType -> Parser TokenType
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

-}
symbol : (Char -> Bool) -> a -> Parser a
symbol predicate context =
    Parser <|
        \((State { source, offset, tokens }) as initialState) ->
            let
                newOffset =
                    offset + 1

                result =
                    String.slice offset newOffset source

                char =
                    List.head <| String.toList result

                error =
                    Err <|
                        Problem
                            { latestState = initialState
                            , expecting = ExpectingSingle context
                            , offset = offset
                            }
            in
            case char of
                Nothing ->
                    error

                Just char ->
                    if predicate char then
                        let
                            token =
                                Token
                                    { state = context
                                    , position = offset
                                    , value = result
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
                        error


{-| After parsing multiple symbols state contains multiple items in the
'tokens' array. This function allows to merged intermediate tokens into
a single one.

    isNotOperator : Parser TokenType
    isNotOperator =
        let
            context =
                Operator IsNotType
        in
        squash context <|
            sequence
                [ keyword "is" Word
                , spaces AtLeastOne
                , keyword "not" Word
                ]

-}
squash : a -> Parser a -> Parser a
squash context parser =
    Parser <|
        \(State initialState) ->
            case apply (State initialState) parser of
                Ok (State nextState) ->
                    let
                        diffNumber =
                            List.length nextState.tokens - List.length initialState.tokens

                        diffTokens =
                            List.take diffNumber nextState.tokens

                        value =
                            List.foldl (\a b -> a ++ b) "" (List.map (\(Token data) -> data.value) diffTokens)

                        newOffset =
                            initialState.offset + String.length value

                        token =
                            Token
                                { state = context
                                , position = initialState.offset
                                , value = value
                                }
                    in
                    Ok
                        (State
                            { source = initialState.source
                            , offset = newOffset
                            , tokens = token :: initialState.tokens
                            }
                        )

                (Err (Problem { latestState, expecting })) as error ->
                    Err <|
                        Problem
                            { latestState = latestState
                            , expecting =
                                ExpectingList <|
                                    removeDuplicates <|
                                        getExpectingValue expecting
                            , offset = initialState.offset
                            }


{-| Keeps calling given parser as many times as possible. Returns (Ok State) if nothing
was parsed

    nonBreakingWord : Parser TokenType
    nonBreakingWord =
        repeat <| symbol (\c -> c /= ' ') Symbol

-}
repeat : Parser a -> Parser a
repeat parser =
    Parser <|
        \initialState ->
            let
                helper =
                    \nextState prevState initialState ->
                        case apply nextState parser of
                            Ok newState ->
                                {- zeroOrMore may return Ok if nothing was parsed -}
                                if newState == prevState then
                                    Ok newState
                                else
                                    helper newState nextState initialState

                            (Err x) as error ->
                                {- if nothing else can be parsed -}
                                if nextState == initialState then
                                    error
                                else
                                    Ok nextState
            in
            helper initialState initialState initialState


{-| Calls parser one or more times. Returns (Err Problem) if was parsed zero
times.

      oneOrMoreSpaces : Parser TokenType
      oneOrMoreSpaces =
          oneOrMore <| symbol (\c -> c == ' ') Space

-}
oneOrMore : Parser a -> Parser a
oneOrMore (Parser parser) =
    Parser <|
        \initialState ->
            let
                helperFunc minTimesMemo nextState =
                    case parser nextState of
                        Ok state ->
                            helperFunc (minTimesMemo + 1) state

                        (Err x) as error ->
                            if minTimesMemo < 1 then
                                error
                            else if minTimesMemo == 0 then
                                Ok initialState
                            else
                                Ok nextState
            in
            helperFunc 0 initialState


{-| Calls parser zero or more times. Always returns (Ok State).

    zeroOrMoreSpaces : Parser TokenType
    zeroOrMoreSpaces =
        zeroOrMore <| symbol (\c -> c == ' ') Space

-}
zeroOrMore : Parser a -> Parser a
zeroOrMore parser =
    maybeOne <| oneOrMore parser


{-| A parser which always returns new state which equals to the input state.

    let
        parser =
            if True then
                symbol (\c -> c == '.') Dot
            else
                identity
    in
    apply state parser

-}
identity : Parser a
identity =
    Parser <| \state -> Ok state


{-| Calls given parser exactly once. In case of fail returns initial state.

    number : Parser TokenType
    number =
        sequence
            [ maybeOne <|
                oneOf
                    [ plus
                    , minus
                    ]
            , digits
            ]

-}
maybeOne : Parser a -> Parser a
maybeOne parser =
    Parser <|
        \initialState ->
            case apply initialState parser of
                (Ok nextState) as result ->
                    result

                Err _ ->
                    Ok initialState


{-| Calls sequence of parsers in the defined order. Returns Ok in case if all
parsers returned successful result

    sequence
        [ keyword "is" context
        , space
        , keyword "not" context
        , space
        , keyword "in" context
        ]

-}
sequence : List (Parser a) -> Parser a
sequence parsers =
    Parser <|
        \initialState ->
            let
                helper latestState parsers =
                    case parsers of
                        [] ->
                            Ok latestState

                        (Parser nextParser) :: restParsers ->
                            case nextParser latestState of
                                Ok nextState ->
                                    helper nextState restParsers

                                (Err _) as result ->
                                    result
            in
            helper initialState parsers


{-| Tries to parse one by one list of parsers and fails if non of the them were
successful
-}
oneOf : List (Parser a) -> Parser a
oneOf parsers =
    Parser <|
        \((State { offset }) as initialState) ->
            let
                helper parsers result =
                    case parsers of
                        [] ->
                            Err <|
                                Problem
                                    { latestState = initialState
                                    , expecting = ExpectingList <| removeDuplicates result
                                    , offset = offset
                                    }

                        parser :: restParsers ->
                            case apply initialState parser of
                                (Ok nextState) as result ->
                                    result

                                (Err (Problem x)) as error ->
                                    if offset == x.offset then
                                        helper restParsers <| List.append result <| getExpectingValue x.expecting
                                    else
                                        error
            in
            helper parsers []


getExpectingValue : Expecting a -> List a
getExpectingValue expecting =
    case expecting of
        ExpectingSingle value ->
            [ value ]

        ExpectingList list ->
            list

        ExpectingEnd ->
            []


removeDuplicates : List a -> List a
removeDuplicates list =
    let
        predicate =
            \item memo ->
                if List.member item memo then
                    memo
                else
                    item :: memo
    in
    List.foldl predicate [] list
