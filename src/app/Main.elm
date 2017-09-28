module Main exposing (..)

import Html exposing (Html, beginnerProgram, button, div, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Styles as Css exposing (..)


type ApplicationState
    = Initialized
    | Started
    | Stopped


type Msg
    = Start
    | Stop
    | Initialize


type alias ApplicationModel =
    { state : ApplicationState
    }


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init =
    ( ApplicationModel Initialized, Cmd.none )


update msg model =
    case msg of
        Initialize ->
            ( { model | state = Initialized }, Cmd.none )

        Start ->
            ( { model | state = Started }, Cmd.none )

        Stop ->
            ( { model | state = Stopped }, Cmd.none )


subscriptions model =
    Sub.none


view model =
    div [ style Css.app ]
        [ getApplicationContent model ]


getApplicationContent model =
    case model.state of
        Initialized ->
            div []
                [ button [ onClick Start ] [ text "Start application!" ]
                ]

        Started ->
            div []
                [ button [ onClick Stop ] [ text "Stop it!" ]
                ]

        Stopped ->
            div []
                [ button [ onClick Start ] [ text "Start it again stupid!" ]
                ]
