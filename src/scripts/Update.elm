module Update exposing (..)

import Actions exposing (Msg(..))
import Lexer
import Model exposing (Model)
import Ports exposing (inputChangeEvent, keyDownEvent)
import TokenEncoder


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        GetCaretPosition ->
            ( model, keyDownEvent "" )

        Parse newValue ->
            let
                tokens =
                    Lexer.evaluate newValue model

                result =
                    TokenEncoder.encodeTokens tokens
            in
            ( { model | value = newValue }, inputChangeEvent result )

        EnterKeyPressed ->
            Debug.log "EnterKeyPressed" (model ! [])

        ArrowUpPressed ->
            Debug.log "ArrowUpPressed" (model ! [])

        ArrowDownPressed ->
            Debug.log "Update:ArrowDownPressed" ( model, Cmd.none )

        TabKeyPressed ->
            Debug.log "TabKeyPressed " (model ! [])
