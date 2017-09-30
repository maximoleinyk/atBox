port module View exposing (..)

import Actions exposing (..)
import Html exposing (Html, div, input, label, text)
import Html.Attributes exposing (class, for, id, placeholder, type_, value)
import Html.Events exposing (onInput)


view model =
    div [ class "at-box" ]
        [ label [ for model.id ] [ text model.label ]
        , input
            [ type_ "text"
            , id model.id
            , value model.value
            , placeholder model.placeholder
            , onInput ParseString
            ]
            []
        ]
