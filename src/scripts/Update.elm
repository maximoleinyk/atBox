module Update exposing (..)

import Actions exposing (Msg(..))
import Model exposing (Model)
import Parser exposing (parse)


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        Parse newValue ->
            let
                parsedResult =
                    parse newValue

                _ =
                    Debug.log "ParsedResult" parsedResult
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
