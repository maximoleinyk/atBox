module Update exposing (..)

import Actions exposing (Msg(..))
import Lexer
import Model exposing (Model)


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        Parse newValue ->
            let
                parsedResult =
                    Lexer.evaluate newValue model

                _ =
                    Debug.log "" parsedResult
            in
            { model | value = newValue } ! []

        EnterKeyPressed ->
            Debug.log "EnterKeyPressed" (model ! [])

        ArrowUpPressed ->
            Debug.log "ArrowUpPressed" (model ! [])

        ArrowDownPressed ->
            Debug.log "Update:ArrowDownPressed" ( model, Cmd.none )

        TabKeyPressed ->
            Debug.log "TabKeyPressed " (model ! [])
