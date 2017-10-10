module Update exposing (..)

import Actions exposing (Msg(..))
import CursorPosition exposing (CursorPosition(NoContext))
import Encoders exposing (encodeFsmResponse, encodeLexemes, encodeTokens)
import FsmResponse exposing (FsmResponse)
import Json.Encode exposing (object, string)
import Lexer exposing (Lexeme)
import Model exposing (Model)
import Parser exposing (AST(Nil))
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
                --                    Debug.log (toString tokens) ""
                contextAtCursorPosition =
                    NoContext

                lexemes =
                    Lexer.run tokens model

                --
                --                b =
                --                    Debug.log (toString lexemes) ""
                ast =
                    Parser.run lexemes model

                translatedOutput =
                    Translator.run ast model

                --
                --                c =
                --                    Debug.log (toString ast) ""
                result =
                    encodeFsmResponse (FsmResponse tokens lexemes ast translatedOutput)
            in
            ( { model
                | value = newValue
                , cursorPosition = contextAtCursorPosition
              }
            , emitData result
            )

        _ ->
            ( model, Cmd.none )
