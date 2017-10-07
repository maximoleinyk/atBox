module Update exposing (..)

import Actions exposing (Msg(..))
import Json.Encode exposing (object, string)
import Lexeme exposing (Lexeme)
import LexemeEncoder
import Lexer
import Model exposing (Model)
import Ports exposing (inputChangeEvent, keyDownEvent)
import Token exposing (Token)
import TokenEncoder
import Tokenizer


type alias FsmResponse =
    { tokens : List Token
    , lexemes : List Lexeme
    }


encodeFsmResponse : FsmResponse -> String
encodeFsmResponse response =
    Json.Encode.encode 2
        (object
            [ ( "tokens", TokenEncoder.encode response.tokens )
            , ( "lexemes", LexemeEncoder.encode response.lexemes )
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

                lexemes =
                    Lexer.evaluate tokens model

                result =
                    encodeFsmResponse (FsmResponse tokens lexemes)
            in
            ( { model | value = newValue }, inputChangeEvent result )

        _ ->
            ( model, Cmd.none )
