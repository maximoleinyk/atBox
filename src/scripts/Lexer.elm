module Lexer exposing (evaluate)

import Dict exposing (Dict)
import Fsm
import FsmState exposing (FsmState(..))
import Model exposing (Model)
import Token exposing (Token)
import Tokenizer


return : String -> Model -> FsmState -> (String -> Model -> String) -> List FsmState -> Dict String String -> FsmState -> List Token
return string model state parser queue newMapping parentState =
    let
        result =
            Tokenizer.tokenize string parser model

        newQueue =
            \n q -> List.drop n q
    in
    if result.length == -1 then
        case state of
            OpenParenthesisTerm ->
                let
                    numberOfStatesToDrop =
                        if parentState == Criterion then
                            1
                        else
                            2
                in
                walk string model (newQueue numberOfStatesToDrop queue) newMapping parentState

            KeywordTerm ->
                walk string model (UnknownKeywordTerm :: queue) newMapping parentState

            StartQuoteTerm ->
                walk string model (newQueue 2 queue) newMapping parentState

            EitherTerm ->
                walk string model (newQueue 6 queue) newMapping parentState

            NeitherTerm ->
                walk string model (newQueue 6 queue) newMapping parentState

            _ ->
                walk string model queue newMapping parentState
    else
        [ Token state result ] ++ walk result.remainingString model queue newMapping parentState


process : String -> Model -> FsmState -> List FsmState -> Dict String String -> FsmState -> List Token
process string model state queue loopDetectionDict parentState =
    let
        -- string neither exists or not equals in the loopDetectionDict
        newMapping =
            Dict.insert (toString state) string loopDetectionDict
    in
    case state of
        CommaTerm ->
            return string model state Tokenizer.comma queue newMapping parentState

        CloseParenthesisTerm ->
            return string model state Tokenizer.closeParenthesis queue newMapping parentState

        OpenParenthesisTerm ->
            return string model state Tokenizer.openParenthesis queue newMapping parentState

        InTerm ->
            return string model state Tokenizer.inTerm queue newMapping parentState

        SpaceTerm ->
            return string model state Tokenizer.space queue newMapping parentState

        WordTerm ->
            return string model state Tokenizer.word queue newMapping parentState

        KeywordTerm ->
            return string model state Tokenizer.keyword queue newMapping parentState

        UnknownKeywordTerm ->
            return string model state Tokenizer.unknownKeyword queue newMapping parentState

        StartQuoteTerm ->
            return string model state Tokenizer.startQuote queue newMapping parentState

        EndQuoteTerm ->
            return string model state Tokenizer.endQuote queue newMapping parentState

        AndTerm ->
            return string model state Tokenizer.and queue newMapping parentState

        OrTerm ->
            return string model state Tokenizer.or queue newMapping parentState

        NorTerm ->
            return string model state Tokenizer.nor queue newMapping parentState

        NotTerm ->
            return string model state Tokenizer.not queue newMapping parentState

        IsTerm ->
            return string model state Tokenizer.is queue newMapping parentState

        EitherTerm ->
            return string model state Tokenizer.either queue newMapping parentState

        NeitherTerm ->
            return string model state Tokenizer.neither queue newMapping parentState

        _ ->
            walk string model queue newMapping state


walk : String -> Model -> List FsmState -> Dict String String -> FsmState -> List Token
walk string model queue loopDetectionDict parentState =
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
                        Fsm.getPossibleStates state

                    -- prepend states of the current state to the rest
                    newStatesQueue =
                        possibleStates ++ rest

                    -- get previous state of the entry when we were in this state
                    previousString =
                        Dict.get (toString state) loopDetectionDict

                    _ =
                        Debug.log (toString state) newStatesQueue
                in
                case previousString of
                    Nothing ->
                        -- proceed if string does not exist
                        process string model state newStatesQueue loopDetectionDict parentState

                    Just previousString ->
                        if previousString == string then
                            -- if both strings are equal - we are inside infinite loop - try to move to the next state
                            walk string model rest loopDetectionDict state
                        else
                            -- strings are different - there is a chance that we might on a correct branch
                            process string model state newStatesQueue loopDetectionDict parentState


evaluate : String -> Model -> List Token
evaluate string model =
    let
        initialState =
            Start

        loopDetection =
            Dict.empty
    in
    walk string model [ initialState ] loopDetection Start
