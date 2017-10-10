module Update exposing (..)

import Actions exposing (Msg(..))
import CursorPosition exposing (CursorPosition(NoContext))
import Encoders exposing (encodeFsmResponse, encodeLexemes, encodeTokens)
import FsmResponse exposing (FsmResponse)
import Json.Encode exposing (object, string)
import Lexer exposing (Lexeme)
import Model exposing (Model)
import Parser exposing (AST(Null))
import Ports exposing (..)
import Tokenizer exposing (Token)
import Translator


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        GetCaretPosition ->
            ( model, getCursorPosition "" )

        UpdateCaretIndex newCursorIndex ->
            ( { model | cursorIndex = newCursorIndex }, Cmd.none )

        Parse newValue ->
            let
                tokens =
                    Tokenizer.run newValue model

                --                a =
                --                    Debug.log "Tokens:" (toString tokens)
                contextAtCursorPosition =
                    NoContext

                lexemes =
                    Lexer.run tokens model

                --
                --                b =
                --                    Debug.log "Lexemes" (toString lexemes)
                ast =
                    Parser.run lexemes model

                --
                --                c =
                --                    Debug.log "AST" (toString ast)
                output =
                    Translator.run ast model

                --
                --                d =
                --                    Debug.log "Output" (toString output)
                result =
                    encodeFsmResponse (FsmResponse tokens lexemes ast output)
            in
            ( { model
                | value = newValue
                , cursorPosition = contextAtCursorPosition
              }
            , emitData result
            )

        _ ->
            ( model, Cmd.none )
