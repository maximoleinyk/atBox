module Update exposing (update)

import ContextAnalyzer
import Dom
import Encoders
import GlobalTypes exposing (FsmResponse, Model, Msg(EnterKeyPressed, Focus, FocusResult, GetCursorPosition, Init, Process, SelectHighlightedValue, UpdateCursorPosition, UpdateValue))
import Lexer
import Parser
import Ports exposing (emitData, emitDataOnEnterKey, getCursorPosition)
import Task
import Tokenizer
import Translator


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        getOutput =
            let
                ( tokens, remainingStates ) =
                    Tokenizer.run model.value model

                lexemes =
                    Lexer.run tokens model

                context =
                    ContextAnalyzer.run model.value tokens lexemes model remainingStates

                ast =
                    Parser.run lexemes model

                output =
                    Translator.run ast model

                result =
                    FsmResponse tokens lexemes ast output
            in
            ( context, result )
    in
    case msg of
        Init ->
            update Process model

        Focus ->
            update GetCursorPosition { model | focused = model.autoSuggest }

        GetCursorPosition ->
            ( model, getCursorPosition "" )

        UpdateCursorPosition newCursorIndex ->
            update Process { model | cursorIndex = newCursorIndex }

        UpdateValue newValue ->
            update GetCursorPosition { model | value = newValue }

        Process ->
            let
                ( context, result ) =
                    getOutput

                command =
                    emitData (Encoders.encodeFsmResponse result)
            in
            ( { model | context = context }, command )

        SelectHighlightedValue data ->
            let
                stringLength =
                    String.length model.value

                stringBeforeIndex =
                    String.slice 0 model.cursorIndex model.value

                stringAfterIndex =
                    String.slice model.cursorIndex stringLength model.value

                newValue =
                    stringBeforeIndex ++ data.replacementValue ++ stringAfterIndex

                command =
                    Task.attempt FocusResult (Dom.focus model.id)
            in
            ( { model | value = newValue }, command )

        EnterKeyPressed ->
            let
                ( context, result ) =
                    getOutput

                command =
                    emitDataOnEnterKey (Encoders.encodeFsmResponse result)
            in
            ( { model | context = context }, command )

        _ ->
            ( model, Cmd.none )
