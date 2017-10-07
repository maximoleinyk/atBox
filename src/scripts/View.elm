module View exposing (view)

import Actions exposing (..)
import CursorPosition exposing (CursorPosition(..))
import Html exposing (Attribute, Html, div, input, label, li, text)
import Html.Attributes exposing (class, for, id, placeholder, type_, value)
import Html.Events exposing (keyCode, on, onFocus, onInput, onWithOptions, targetValue)
import Html.Keyed exposing (ul)
import Json.Decode exposing (andThen, at, fail, map, string, succeed)
import Model exposing (Model)
import Operator exposing (Operator)
import QueryField exposing (QueryField)
import String exposing (contains, toLower)


onCaretPositionUpdate : Attribute Msg
onCaretPositionUpdate =
    on "keyup" (succeed GetCaretPosition)


onKeyHandle : Model -> Attribute Msg
onKeyHandle model =
    let
        options =
            Html.Events.Options False True

        handleKeyPress code =
            case code of
                13 ->
                    succeed EnterKeyPressed

                9 ->
                    succeed TabKeyPressed

                38 ->
                    succeed ArrowUpPressed

                40 ->
                    if model.cursorPosition /= NoContext then
                        succeed ArrowDownPressed
                    else
                        fail "do nothing"

                _ ->
                    fail "unsupported"
    in
    onWithOptions "keydown" options (andThen handleKeyPress keyCode)


view : Model -> Html Msg
view model =
    div [ class "at-box" ]
        [ div [ class "at-box-group" ]
            [ label [ for model.id ] [ text model.label ]
            , input
                [ type_ "text"
                , id model.id
                , value model.value
                , placeholder model.placeholder
                , onInput Parse
                , onKeyHandle model
                , onCaretPositionUpdate
                ]
                []
            ]
        , div [ class "at-box-list" ]
            [ getSuggestions model ]
        ]


getSuggestions : Model -> Html Msg
getSuggestions model =
    let
        matchPatternFilter =
            List.filter (\op -> contains (toLower model.currentToken) (toLower op.label))
    in
    case model.cursorPosition of
        NoContext ->
            text ""

        -- Ex: find a person whose @____
        AfterAtSymbol ->
            ul []
                (model.queryFields
                    |> matchPatternFilter
                    |> List.map queryLi
                )

        -- Ex: find a person whose @name ____
        AfterAtField ->
            ul []
                (model.operators
                    -- exclude OR & AND operators
                    |> List.filter (\op -> not op.conjunction)
                    -- match result
                    |> matchPatternFilter
                    -- produce HTML
                    |> List.map operatorLi
                )

        -- Ex: find a person whose @name is Max ____
        AfterOperatorAndValue ->
            ul []
                (model.operators
                    -- exclude everything except OR & AND operators
                    |> List.filter (\op -> op.conjunction)
                    -- match result
                    |> matchPatternFilter
                    -- produce HTML
                    |> List.map operatorLi
                )


queryLi : QueryField -> ( String, Html msg )
queryLi queryField =
    ( queryField.field, li [] [ text queryField.label ] )


operatorLi : Operator -> ( String, Html msg )
operatorLi operator =
    ( operator.symbol, li [] [ text operator.label ] )
