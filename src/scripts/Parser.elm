module Parser exposing (parse)

import Dict exposing (Dict)
import FsmState exposing (FsmState(..))
import ParsedResult exposing (ParsedResult)
import Parsers


type alias ParsedToken =
    { state : FsmState
    , string : String
    }


getPossibleStates : FsmState -> List FsmState
getPossibleStates state =
    case state of
        Start ->
            [ Statement ]

        Statement ->
            [ Space, Word, Statement ]

        Space ->
            []

        Word ->
            []


process : FsmState -> String -> List FsmState -> Dict String String -> List ParsedToken
process state string queue loopDetectionDict =
    let
        newMapping =
            Dict.insert (toString state) string loopDetectionDict
    in
    case state of
        Start ->
            walk string queue newMapping

        Statement ->
            walk string queue newMapping

        Space ->
            let
                result =
                    Parsers.parse string Parsers.space
            in
            if result.length == -1 then
                walk string queue newMapping
            else
                [ ParsedToken state result.string ] ++ walk result.newString queue newMapping

        Word ->
            let
                result =
                    Parsers.parse string Parsers.word
            in
            if result.length == -1 then
                walk string queue newMapping
            else
                [ ParsedToken state result.string ] ++ walk result.newString queue newMapping


walk : String -> List FsmState -> Dict String String -> List ParsedToken
walk string queue loopDetectionDict =
    -- empty string means we finished parsing
    if string == "" then
        []
    else
        -- asses queue of upcoming states
        case queue of
            -- is queue is empty it means we finished parsing
            [] ->
                []

            -- process first state
            state :: rest ->
                let
                    -- get list of next states
                    possibleStates =
                        getPossibleStates state

                    -- prepend states of the current state to the rest
                    newStatesQueue =
                        possibleStates ++ rest

                    -- get previous state of the entry when we were in this state
                    previousString =
                        Dict.get (toString state) loopDetectionDict
                in
                case previousString of
                    -- proceed if string does not exist
                    Nothing ->
                        process state string newStatesQueue loopDetectionDict

                    Just previousString ->
                        -- if both strings are equal - we are in the infinite loop
                        if previousString == string then
                            []
                        else
                            process state string newStatesQueue loopDetectionDict


parse : String -> List ParsedToken
parse string =
    let
        initialState =
            Start

        loopDetection =
            Dict.empty
    in
    walk string [ initialState ] loopDetection
