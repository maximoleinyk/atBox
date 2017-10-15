module Update exposing (..)

import ContextAnalyzer
import Dom
import Encoders exposing (encodeFsmResponse, encodeLexemes, encodeTokens)
import GlobalTypes exposing (AST, CursorContext, FsmResponse, Lexeme, Model, Msg(Blur, Focus, FocusResult, GetCursorPosition, Parse, SelectHighlightedValue, UpdateCursorPosition, UpdateValue), Token, TokenState, TranslatorOutput)
import Html.Attributes exposing (id)
import Json.Encode exposing (object, string)
import Lexer
import Parser
import Ports exposing (..)
import SyntaxHighlighter
import Task
import Tokenizer
import Translator


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        getTokens : String -> Model -> ( List Token, List TokenState )
        getTokens =
            \value model ->
                Tokenizer.run value model

        getLexemes : List Token -> Model -> List Lexeme
        getLexemes =
            \tokens model ->
                Lexer.run tokens model

        getContext : String -> List Token -> List Lexeme -> Model -> List TokenState -> CursorContext
        getContext =
            \string tokens lexemes model remainingStates ->
                ContextAnalyzer.run string tokens lexemes model remainingStates

        getAST : List Lexeme -> Model -> AST
        getAST =
            \lexemes model ->
                Parser.run lexemes model

        getTranslatorOutput : AST -> Model -> TranslatorOutput
        getTranslatorOutput =
            \ast model ->
                Translator.run ast model

        parse : Model -> ( FsmResponse, CursorContext )
        parse =
            \model ->
                let
                    ( tokens, remainingStates ) =
                        getTokens model.value model

                    lexemes =
                        getLexemes tokens model

                    context =
                        getContext model.value tokens lexemes model remainingStates

                    ast =
                        getAST lexemes model

                    output =
                        getTranslatorOutput ast model

                    a =
                        Debug.log "" output

                    fsmResponse =
                        FsmResponse tokens lexemes ast output ""
                in
                ( fsmResponse, context )
    in
    case msg of
        Focus ->
            update GetCursorPosition { model | focused = True }

        GetCursorPosition ->
            ( model, getCursorPosition "" )

        UpdateCursorPosition newCursorIndex ->
            update Parse { model | cursorIndex = newCursorIndex }

        UpdateValue newValue ->
            update Parse { model | value = newValue }

        Parse ->
            let
                ( result, context ) =
                    parse model

                command =
                    emitData (encodeFsmResponse result)
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

                concatenatedString =
                    stringBeforeIndex ++ data.replacementValue ++ stringAfterIndex

                newModel =
                    { model
                        | value = concatenatedString
                        , cursorIndex = String.length concatenatedString
                    }

                ( result, context ) =
                    parse newModel

                command =
                    Dom.focus model.id |> Task.attempt FocusResult
            in
            ( { newModel | context = context }, command )

        FocusResult result ->
            case result of
                Err (Dom.NotFound id) ->
                    ( model, Cmd.none )

                Ok () ->
                    update (UpdateCursorPosition model.cursorIndex) model

        _ ->
            ( model, Cmd.none )
