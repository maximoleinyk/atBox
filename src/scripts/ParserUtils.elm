module ParserUtils
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
        , symbol
        , word
        , zeroOrMore
        )


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


type Problem a
    = Problem
        { latestState : State a
        , expecting : List a
        , offset : Int
        }


type Parser a
    = Parser (State a -> Result (Problem a) (State a))


apply : State a -> Parser a -> Result (Problem a) (State a)
apply state (Parser parse) =
    parse state


end : Parser a
end =
    Parser <|
        \((State { source, offset }) as initialState) ->
            if String.length source /= offset then
                Err <|
                    Problem
                        { latestState = initialState
                        , expecting = []
                        , offset = offset
                        }
            else
                Ok initialState


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
                    Err <|
                        Problem
                            { latestState = initialState
                            , expecting = [ context ]
                            , offset = offset
                            }

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
                        Err <|
                            Problem
                                { latestState = initialState
                                , expecting = [ context ]
                                , offset = offset
                                }


word : a -> Parser a
word context =
    Parser <|
        \((State { source, offset, tokens }) as initialState) ->
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
                    token =
                        Token
                            { state = context
                            , position = offset
                            , value = String.slice offset newOffset source
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
                Err <|
                    Problem
                        { latestState = initialState
                        , expecting = [ context ]
                        , offset = offset
                        }


squash : a -> Parser a -> Parser a
squash context parser =
    Parser <|
        \(State initialState) ->
            case apply (State initialState) parser of
                Ok (State nextState) ->
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
                    Ok
                        (State
                            { source = initialState.source
                            , offset = newOffset
                            , tokens = token :: initialState.tokens
                            }
                        )

                (Err (Problem { latestState })) as error ->
                    Err <|
                        Problem
                            { latestState = latestState
                            , expecting = [ context ]
                            , offset = initialState.offset
                            }


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


zeroOrMore : Parser a -> Parser a
zeroOrMore parser =
    maybeOne <| oneOrMore parser


identity : Parser a
identity =
    Parser <| \state -> Ok state


maybeOne : Parser a -> Parser a
maybeOne parser =
    Parser <|
        \initialState ->
            case apply initialState parser of
                (Ok nextState) as result ->
                    result

                Err _ ->
                    Ok initialState


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
                                    , expecting = result
                                    , offset = offset
                                    }

                        parser :: restParsers ->
                            case apply initialState parser of
                                (Ok nextState) as result ->
                                    result

                                (Err (Problem x)) as error ->
                                    if offset == x.offset then
                                        helper restParsers <| List.append result x.expecting
                                    else
                                        error
            in
            helper parsers []
