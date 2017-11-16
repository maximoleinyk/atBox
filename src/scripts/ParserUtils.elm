module Parser
    exposing
        ( Parser(..)
        , State(..)
        , Token(..)
        , apply
        , keyword
        , maybeOne
        , oneOf
        , oneOrMore
        , repeat
        , sequence
        , squash
        , swapBy
        , symbol
        , zeroOrMore
        )

import Char


type Token a
    = Token
        { state : a
        , position : Int
        , value : String
        }


type State a
    = State
        { source : String
        , offset : Int
        , tokens : List (Token a)
        }


type Parser a
    = Parser (State a -> Result String (State a))


apply : State a -> Parser a -> Result String (State a)
apply state (Parser parse) =
    parse state


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
            in
            case char of
                Nothing ->
                    Err "Cannot parse symbol"

                Just char ->
                    if predicate char then
                        Ok
                            (State
                                { source = source
                                , offset = newOffset
                                , tokens = Token { state = context, position = offset, value = result } :: tokens
                                }
                            )
                    else
                        Err <| "Could not parse symbol: " ++ toString char


keyword : String -> a -> Parser a
keyword key context =
    Parser <|
        \initialState ->
            let
                mapper =
                    \c -> symbol (\char -> Char.toLower char == c) context

                result =
                    apply initialState <|
                        sequence (List.map mapper (String.toList (String.toLower key)))
            in
            case result of
                Ok newState ->
                    Ok (squash initialState newState context)

                (Err _) as result ->
                    result


squash : State a -> State a -> a -> State a
squash (State initialState) (State nextState) context =
    let
        diff =
            List.length nextState.tokens - List.length initialState.tokens

        diffTokens =
            List.take diff nextState.tokens

        value =
            List.foldl (\a b -> a ++ b) "" (List.map (\(Token data) -> data.value) diffTokens)

        newOffset =
            initialState.offset + String.length value

        token =
            Token { state = context, position = initialState.offset, value = value }
    in
    State
        { source = initialState.source
        , offset = newOffset
        , tokens = token :: initialState.tokens
        }


swapByHelper : (a -> Bool) -> Parser a -> Parser a -> State a -> State a -> State a -> Result String (State a)
swapByHelper condition parserTrue parserFalse ((State { tokens }) as nextState) prevState initialState =
    case List.head tokens of
        Just (Token { state }) ->
            if condition state then
                case apply nextState parserTrue of
                    Ok value ->
                        if value == prevState then
                            Ok value
                        else
                            swapByHelper condition parserTrue parserFalse value nextState initialState

                    Err x ->
                        if nextState == initialState then
                            Err "flip: parse failed"
                        else
                            Ok nextState
            else
                case apply nextState parserFalse of
                    Ok value ->
                        if value == prevState then
                            Ok value
                        else
                            swapByHelper condition parserTrue parserFalse value nextState initialState

                    Err x ->
                        Ok nextState

        Nothing ->
            case apply initialState parserFalse of
                Ok value ->
                    if value == initialState then
                        Ok value
                    else
                        swapByHelper condition parserTrue parserFalse value initialState initialState

                Err x ->
                    Err "flip: parse failed"


swapBy : (a -> Bool) -> Parser a -> Parser a -> Parser a
swapBy condition parserTrue parserFalse =
    Parser <|
        \state ->
            swapByHelper condition parserTrue parserFalse state state state


runParserNTimes : Int -> Parser a -> State a -> Result String (State a)
runParserNTimes requiredAmountOfTimes (Parser parse) initialState =
    let
        helperFunc minTimesMemo nextState =
            case parse nextState of
                Ok state ->
                    helperFunc (minTimesMemo + 1) state

                Err _ ->
                    if minTimesMemo < requiredAmountOfTimes then
                        Err ("Parse failed at least " ++ toString requiredAmountOfTimes ++ " times")
                    else if minTimesMemo == 0 then
                        Ok initialState
                    else
                        Ok nextState
    in
    helperFunc 0 initialState


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

                            Err reason ->
                                {- if nothing else can be parsed -}
                                if nextState == initialState then
                                    Err reason
                                else
                                    Ok nextState
            in
            helper initialState initialState initialState


maybeOne : Parser a -> Parser a
maybeOne parser =
    Parser <|
        \initialState ->
            case apply initialState parser of
                (Ok nextState) as result ->
                    result

                Err _ ->
                    Ok initialState


oneOrMore : Parser a -> Parser a
oneOrMore parser =
    Parser <|
        \initialState ->
            runParserNTimes 1 parser initialState


zeroOrMore : Parser a -> Parser a
zeroOrMore parser =
    Parser <|
        \initialState ->
            runParserNTimes 0 parser initialState


sequence : List (Parser a) -> Parser a
sequence parsers =
    Parser <|
        \initialState ->
            let
                parseNext latestState parsers =
                    case parsers of
                        [] ->
                            Ok latestState

                        (Parser nextParser) :: restParsers ->
                            case nextParser latestState of
                                Ok nextState ->
                                    parseNext nextState restParsers

                                Err _ ->
                                    Err "sequence: parse failed"
            in
            parseNext initialState parsers


oneOf : List (Parser a) -> Parser a
oneOf parsers =
    Parser <|
        \initialState ->
            let
                helper parsers =
                    case parsers of
                        [] ->
                            Err "oneOf: parse failed"

                        (Parser parse) :: restParsers ->
                            case parse initialState of
                                Err _ ->
                                    helper restParsers

                                (Ok nextState) as result ->
                                    result
            in
            helper parsers
