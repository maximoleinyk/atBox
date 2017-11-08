module ParserUtils
    exposing
        ( (|=)
        , Parser
        , State
        , Step
        , Token(..)
        , apply
        , identity
        , keyword
        , oneOf
        , oneOrMore
        , sentence
        , sequence
        , space
        , squash
        , symbol
        , word
        , zeroOrMore
        )


type Token
    = BoolToken Bool
    | CharToken Char
    | StringToken String
    | WordToken String
    | KeywordToken String


type Step
    = Step
        { position : Int
        , value : String
        }
        Token


type State
    = State
        { source : String
        , offset : Int
        , tokens : List Step
        }


type Parser
    = Parser (State -> Result String State)


symbol : Char -> Parser
symbol char =
    Parser <|
        \((State { source, offset, tokens }) as initialState) ->
            let
                newOffset =
                    offset + 1

                result =
                    String.slice offset newOffset source
            in
            if result == String.fromChar char then
                Ok
                    (State
                        { source = source
                        , offset = newOffset
                        , tokens = Step { position = offset, value = result } (CharToken char) :: tokens
                        }
                    )
            else
                Err ("Could not parse symbol: " ++ toString char)


squash : State -> State -> (String -> Token) -> State
squash (State initialState) (State newState) getToken =
    let
        diff =
            List.length newState.tokens - List.length initialState.tokens

        diffTokens =
            List.take diff newState.tokens

        value =
            List.foldl (\a b -> a ++ b) "" (List.map (\(Step ctx value) -> ctx.value) diffTokens)

        newOffset =
            initialState.offset + String.length value
    in
    State
        { source = initialState.source
        , offset = newOffset
        , tokens = Step { position = initialState.offset, value = value } (getToken value) :: initialState.tokens
        }


runParserNTimes : Int -> Parser -> State -> Result String State
runParserNTimes requiredAmountOfTimes (Parser parse) ((State { source, offset, tokens }) as initialState) =
    let
        helperFunc minTimesMemo nextState =
            case parse nextState of
                Ok state ->
                    helperFunc (minTimesMemo + 1) state

                Err _ ->
                    if minTimesMemo < requiredAmountOfTimes then
                        Err ("Could not parse at least " ++ toString requiredAmountOfTimes ++ " times")
                    else if minTimesMemo == 0 then
                        Ok initialState
                    else
                        Ok (squash initialState nextState (\value -> StringToken value))
    in
    helperFunc 0 initialState


oneOrMore : Parser -> Parser
oneOrMore parser =
    Parser <|
        \((State { source, offset, tokens }) as initialState) ->
            runParserNTimes 1 parser initialState


zeroOrMore : Parser -> Parser
zeroOrMore parser =
    Parser <|
        \((State { source, offset, tokens }) as initialState) ->
            runParserNTimes 0 parser initialState


identity : Parser
identity =
    Parser <| \s -> Ok s


apply : Parser -> State -> Result String State
apply (Parser parse) state =
    parse state


sequence : List Parser -> Parser
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
                                    Err "could not parse sequence"
            in
            parseNext initialState parsers


oneOf : List Parser -> Parser
oneOf parsers =
    Parser <|
        \initialState ->
            let
                helper parsers =
                    case parsers of
                        [] ->
                            Err "Could not parse non of the available parsers"

                        (Parser parse) :: restParsers ->
                            case parse initialState of
                                Err _ ->
                                    helper restParsers

                                (Ok nextState) as result ->
                                    result
            in
            helper parsers


space : Parser
space =
    symbol ' '


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
                in
                Ok
                    (State
                        { source = source
                        , offset = newOffset
                        , tokens = Step { position = offset, value = value } (WordToken value) :: tokens
                        }
                    )
            else
                Err "Could not parse a word"


sentence : Parser
sentence =
    Parser <|
        \initialState ->
            let
                helper nextState prevState =
                    let
                        parser =
                            oneOf
                                [ word
                                , oneOrMore space
                                ]
                    in
                    case apply parser nextState of
                        Ok newState ->
                            {- zeroOrMore may return Ok if nothing was parsed -}
                            if newState == prevState then
                                Ok newState
                            else
                                helper newState nextState

                        Err reason ->
                            {- if nothing else can be parsed -}
                            if nextState == initialState then
                                Err reason
                            else
                                Ok nextState
            in
            helper initialState initialState


keyword : String -> Parser
keyword key =
    Parser <|
        \initialState ->
            let
                parser =
                    sequence
                        [ symbol '@'
                        , sequence (List.map (\c -> symbol c) (String.toList key))
                        ]

                result =
                    apply parser initialState
            in
            case result of
                Ok newState ->
                    Ok (squash initialState newState (\value -> KeywordToken value))

                (Err _) as result ->
                    result


(|=) : Parser -> Parser -> Parser
(|=) (Parser a) (Parser b) =
    Parser <|
        \state ->
            let
                _ =
                    Debug.log "state" state
            in
            case a state of
                Ok nextState ->
                    b nextState

                Err _ ->
                    apply identity state


run : String -> ()
run source =
    let
        initialState =
            State
                { source = source
                , offset = 0
                , tokens = []
                }

        parser =
            identity
                |= sentence
                |= keyword "name"

        result =
            apply parser initialState

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
