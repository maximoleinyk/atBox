port module Main exposing (..)

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
    ( ApplicationModel Initialized, logger "Initialized" )


update msg model =
    case msg of
        Start ->
            ( { model | state = Started }, logger "Started" )

        Stop ->
            ( { model | state = Stopped }, logger "Stopped" )


subscriptions : ApplicationModel -> Sub Msg
subscriptions model =
    bus
        (\message ->
            case message of
                "Start" ->
                    Start

                "Stop" ->
                    Stop

                _ ->
                    Stop
        )


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


port logger : String -> Cmd msg


port bus : (String -> msg) -> Sub msg
