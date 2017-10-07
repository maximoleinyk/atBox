module Update exposing (..)

import Actions exposing (Msg(..))
import CursorPosition exposing (CursorPosition(NoContext))
import Encoders exposing (encodeLexemes, encodeTokens)
import Json.Encode exposing (object, string)
import Lexer exposing (Lexeme)
import Model exposing (Model)
import Ports exposing (inputChangeEvent, keyDownEvent)
import Tokenizer exposing (Token)


type alias FsmResponse =
    { tokens : List Token
    , lexemes : List Lexeme
    }


encodeFsmResponse : FsmResponse -> String
encodeFsmResponse response =
    Json.Encode.encode 2
        (object
            [ ( "tokens", encodeTokens response.tokens )
            , ( "lexemes", encodeLexemes response.lexemes )
            ]
        )


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        GetCaretPosition ->
            ( model, keyDownEvent "" )

        Parse newValue ->
            let
                tokens =
                    Tokenizer.run newValue model

                contextAtCursorPosition =
                    NoContext

                lexemes =
                    Lexer.run tokens model

                --                a =
                --                    Debug.log (toString tokens) ""
                --
                --                b =
                --                    Debug.log (toString lexemes) ""
                result =
                    encodeFsmResponse (FsmResponse tokens lexemes)
            in
            ( { model
                | value = newValue
                , cursorPosition = contextAtCursorPosition
              }
            , inputChangeEvent result
            )

        _ ->
            ( model, Cmd.none )
