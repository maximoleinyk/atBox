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


return : String -> FsmState -> (List String -> List String) -> List FsmState -> Dict String String -> List ParsedToken
return string state parser queue newMapping =
    let
        result =
            Parsers.parse string parser
    in
    if result.length == -1 then
        walk string queue newMapping
    else
        [ ParsedToken state result.string ] ++ walk result.newString queue newMapping


process : String -> FsmState -> List FsmState -> Dict String String -> List ParsedToken
process string state queue loopDetectionDict =
    let
        -- string neither exists or not equals in the loopDetectionDict
        newMapping =
            Dict.insert (toString state) string loopDetectionDict
    in
    case state of
        Start ->
            walk string queue newMapping

        Statement ->
            walk string queue newMapping

        Space ->
            return string state Parsers.space queue newMapping

        Word ->
            return string state Parsers.word queue newMapping


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
                        process string state newStatesQueue loopDetectionDict

                    Just previousString ->
                        -- if both strings are equal - we are inside infinite loop
                        if previousString == string then
                            []
                        else
                            process string state newStatesQueue loopDetectionDict


parse : String -> List ParsedToken
parse string =
    let
        initialState =
            Start

        loopDetection =
            Dict.empty
    in
    walk string [ initialState ] loopDetection
