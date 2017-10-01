module Parser exposing (parse)

import FsmState exposing (FsmState(..))
import ParsedResult exposing (ParsedResult)
import Parsers exposing (noParser, parseSpace, parseWord)


type alias ParsedToken =
    { state : FsmState
    , string : String
    }


getParser : FsmState -> (String -> ParsedResult)
getParser state =
    case state of
        SpaceTerm ->
            parseSpace

        CharTerm ->
            parseWord

        _ ->
            noParser


getPossibleStates : FsmState -> List FsmState
getPossibleStates state =
    case state of
        Start ->
            [ Statement ]

        Statement ->
            [ Space, Word, Statement ]

        Space ->
            [ SpaceTerm ]

        SpaceTerm ->
            []

        Word ->
            [ CharTerm ]

        CharTerm ->
            []


internalParse : String -> FsmState -> List FsmState -> List ParsedToken
internalParse string state possibleStates =
    if string == "" then
        Debug.log "Parsing has been finished" []
    else
        case state of
            Start ->
                case possibleStates of
                    [] ->
                        Debug.log "No possible states has left in the beginning" []

                    nextState :: rest ->
                        let
                            nextPossibleStates =
                                getPossibleStates nextState
                        in
                        if List.length nextPossibleStates /= 0 then
                            internalParse string nextState nextPossibleStates
                        else
                            let
                                parser =
                                    getParser nextState

                                parsedResult =
                                    parser string
                            in
                            -- parser did not do job - try next state
                            if parsedResult.length == -1 then
                                case rest of
                                    [] ->
                                        []

                                    nextNextState :: restRest ->
                                        internalParse string nextNextState restRest
                            else
                                -- proceed with the same parser one more time
                                let
                                    newString =
                                        parsedResult.newString
                                in
                                [ ParsedToken nextState parsedResult.string ] ++ internalParse newString state rest

            Statement ->
                Debug.log "We are in Statement state now" []

            Word ->
                Debug.log "We are in Word state now" []

            CharTerm ->
                let
                    parsedResult =
                        parseWord string
                in
                -- parser did not do job - try next state
                if parsedResult.length == -1 then
                    []
                else
                    [ ParsedToken state parsedResult.string ]

            Space ->
                Debug.log "We are in Space state now" []

            SpaceTerm ->
                let
                    parsedResult =
                        parseSpace string
                in
                -- parser did not do job - try next state
                if parsedResult.length == -1 then
                    []
                else
                    [ ParsedToken state parsedResult.string ]


parse : String -> List ParsedToken
parse string =
    let
        initialState =
            Start

        possibleStates =
            getPossibleStates initialState
    in
    internalParse string initialState possibleStates
